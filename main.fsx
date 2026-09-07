open System

type ValidationResult<'t, 'err> = OK of 't | Error of 'err list


//validation functions

let Exists (value: 'a option) = 
    match value with
    | Some v -> true
    | None -> false

let notEmpty (s: string) = 
    if s = "" then false
    else true

let MaxLengthFunc (s:string) (n:int) =
    if s.Length > n then
        true
    else
        false

let FutureDateFunc (d:DateTime) =
    if d < DateTime.Now then
        false
    else
        true

let rec Levenshtein (s:string) (t:string) = 
    if s = "" then t.Length
    elif t = "" then s.Length
    elif s[(s.Length - 2)..] = t[(t.Length - 2)..] then 
        Levenshtein s[..s.Length - 2] t[..t.Length - 2]  
    else
        1 + List.min 
            [Levenshtein s[..s.Length - 2] t;
            Levenshtein s t[..t.Length - 2];
            Levenshtein s[..s.Length - 2] t[..t.Length - 2]]

let rec MatchStrings (s:string) (names:string list) (strict:bool) : string option =
    if strict = true then
        let nameScores = List.filter (fun name -> (Levenshtein s name) = 0) names
        if nameScores.Length = 0 then
            None
        else
            Some s
    else
        let nameScores = List.filter (fun name -> (Levenshtein s name) < 3) names
        if nameScores.Length = 0 then
            None
        elif nameScores.Length > 1 then
            MatchStrings s names true
        else
            Some s

let RegexMatch (s:string) (expression:string) (ruleName:string) =
    let rx = Text.RegularExpressions.Regex(expression, Text.RegularExpressions.RegexOptions.Compiled)
    let matching = rx.IsMatch(s)
    matching

let CheckPostcode (s:string) = 
    let postcodeRegex = "^([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z])))) [0-9][A-Za-z]{2})$"
    RegexMatch s postcodeRegex "postcode"




//examples

type PersonInput = { 
Name: string option; 
DOB: DateTime option; 
Postcode: string option 
}
type ValidPerson = {
Name: string
DOB: DateTime
Postcode: string option
}


type NameValidations = MustBeEntered | MaxLength of string
type DOBValidations = MustBeEntered | PastDate of DateTime
type PostcodeValidations = ValidPostcode of string
type PersonValidations =
    | Name of NameValidations
    | DOB of DOBValidations
    | Postcode of PostcodeValidations

let validateName (name: string option) : ValidationResult<string, PersonValidations> =
    match name with
    | None -> Error [Name NameValidations.MustBeEntered]
    
    | Some value when (MaxLengthFunc value 70 ) -> 
        Error [ Name (NameValidations.MaxLength value) ]

    | Some value -> OK value

let validateDOB(date: DateTime option) : ValidationResult<DateTime, PersonValidations> = 
    match date with
    | None -> Error [DOB DOBValidations.MustBeEntered]
    | Some value when (FutureDateFunc value) = true ->
        Error [DOB (DOBValidations.PastDate value)]
    | Some value -> OK value

let validatePostcode(postcode: string option) : ValidationResult<string option, PersonValidations> =
    match postcode with
    | None -> OK None
    | Some value when (CheckPostcode value) = false ->
        Error [Postcode (PostcodeValidations.ValidPostcode value)]
    | Some value -> OK (Some value)


let CaseTypesList = ["Drug Abuse";"Anti Social";"Health"]
type CaseInput = {
CaseType: string option
Subject: ValidPerson option
Supervisor: ValidPerson option
}
type ValidCase = {
CaseType: String 
Subject: ValidPerson 
Supervisor: ValidPerson 
}

type CaseTypeValidatoins = MustBeEntered | ValidCase of string
type CaseValidations = CaseType of CaseTypeValidatoins



let ValidateCaseType (name:string option) : ValidationResult<string,CaseValidations> =
    match name with
    | None -> Error [CaseType CaseTypeValidatoins.MustBeEntered]
    | Some value ->
        let output = (MatchStrings value CaseTypesList false ) 
        if output = None then Error [CaseType (CaseTypeValidatoins.ValidCase value)]
        else OK output.Value

let map f result =
    match result with
    | OK x -> OK (f x)
    | Error errors -> Error errors
let (<!>) f result = map f result

let apply fResult xResult =
    match fResult, xResult with
    | OK f, OK x -> OK (f x)
    | Error e1, OK _ -> Error e1
    | OK _, Error e2 -> Error e2
    | Error e1, Error e2 -> Error (e1 @ e2)
let (<*>) fResult xResult = apply fResult xResult

let createPerson name dob postcode : ValidPerson =
    { Name = name; DOB = dob; Postcode = postcode }

let validatePerson (input: PersonInput) =
    createPerson <!> validateName input.Name <*> validateDOB input.DOB <*> validatePostcode input.Postcode

let createCase casetype subject supervisor : ValidCase = 
    { CaseType = casetype; Subject = subject; Supervisor = supervisor }
let validateCase (input: CaseInput) =
    createCase <!> ValidateCaseType input.CaseType 


//tests
let printResult name result =
    match result with
    | OK v -> printfn "%s: PASS %A" name v
    | Error errs -> printfn "%s: FAIL %A" name errs

printResult "person valid" (validatePerson { Name = Some "Jane Doe"; DOB = Some (DateTime(1990,1,1)); Postcode = Some "SW1A 1AA" })
printResult "person missing name+dob" (validatePerson { Name = None; DOB = None; Postcode = Some "SW1A 1AA" })
printResult "person bad postcode" (validatePerson { Name = Some "Jane Doe"; DOB = Some (DateTime(1990,1,1)); Postcode = Some "NOTAPOSTCODE" })
printResult "person future dob" (validatePerson { Name = Some "Jane Doe"; DOB = Some (DateTime.Now.AddDays(5.0)); Postcode = None })

printResult "case valid" (ValidateCaseType (Some "Health"))
printResult "case typo" (ValidateCaseType (Some "Helth"))
printResult "case unknown" (ValidateCaseType (Some "Robbery"))
printResult "case missing" (ValidateCaseType None)
