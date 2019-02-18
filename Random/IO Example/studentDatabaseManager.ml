type student = {
first_name : string; 
last_name : string; 
id : int; 
semester : int; 
grades : (int * float) list}

type database = student list
exception Corrupt_database_file

(* Helper Function I: Reading Grades List*)
let rec read_grades in_ch lines_remaining acc = 
        if lines_remaining <= 0 then List.rev acc
        else 
            try
               let line = input_line in_ch
               in
               match String.split_on_char ';' line with
                | ["";_] | [_;""] -> raise Corrupt_database_file
                | [course_id_s; course_grade_s] ->  
                        let course_id    = try 
                                           int_of_string course_id_s
                                           with _ -> raise Corrupt_database_file
                        in
                        let course_grade = try
                                           float_of_string course_grade_s
                                           with _ -> raise Corrupt_database_file
                        in
                        if course_id < 0 || course_grade < 1.00 || course_grade > 5.00
                        then raise Corrupt_database_file
                        else 
                        read_grades in_ch (lines_remaining-1) ((course_id,course_grade)::acc)
                | _ -> raise Corrupt_database_file
             with End_of_file -> raise Corrupt_database_file
    
(* Helper Function II *)
let rec read_students in_ch acc =
    try
        let line = input_line in_ch
        in
        match String.split_on_char ';' line with
        | ["";_;_;_;_] 
        | [_;"";_;_;_] 
        | [_;_;"";_;_] 
        | [_;_;_;"";_]  
        | [_;_;_;_;""] -> raise Corrupt_database_file
        | [fn;ln;id_s;sem_s;no_of_grades_s] -> 
            let id           = try int_of_string id_s
                               with _ -> raise Corrupt_database_file
            in
            let sem          = try int_of_string sem_s
                               with _ -> raise Corrupt_database_file
            in
            let no_of_grades = try int_of_string no_of_grades_s
                               with _ -> raise Corrupt_database_file
            in
            if id < 0 || sem < 0 || no_of_grades <0 then raise Corrupt_database_file
            else if acc |> List.find_opt (fun s -> s.id = id) <> None (* id twice in db*)
            then raise Corrupt_database_file
            else 
            let grades = read_grades in_ch no_of_grades []
            in
            let student = { first_name = fn;
                            last_name = ln;
                            id = id;
                            semester = sem;
                            grades = grades }
            in
            read_students in_ch (student :: acc)
        | _ -> raise Corrupt_database_file
    with End_of_file -> acc |> List.rev 

(* Reading Function *)
let load_db filename = 
    let in_ch = open_in filename
    in
    try
        let db = read_students in_ch []
        in
        let _ = close_in in_ch
        in
        db
    with e -> let _ = close_in in_ch
              in
              raise e


(* Writing Function *)
let store_db db filename =
    let out_ch = open_out filename
    in
    let write_grade (course_id, course_grade) = 
        Printf.fprintf out_ch "%d;%.2f\n" 
        course_id course_grade
    in
    let write_student s = 
        let _ = Printf.fprintf out_ch "%s;%s;%d;%d;%d\n" 
            s.first_name 
            s.last_name
            s.id
            s.semester
            (List.length s.grades)
        in
        s.grades |> List.iter write_grade
    in
    let _ = db |> List.iter write_student
    in
    close_out out_ch

    
