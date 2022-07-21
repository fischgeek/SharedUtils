namespace Requesty
module Types =

    type StandardErrors =
        | AggregateError of StandardErrors list
        | FailedToGetRecords of exn
        | NestedErrors of string seq
        | AnUnexpectedErrorOccurred of exn
        | AnUnexpectedErrorOccurredDetailed of (string * exn)
        | AnUnexpectedErrorOccurredAsMessage of string
        | InvalidUserProfileID of string
        | RecordNotFoundByID of int
        | RecordNotFoundByID64 of int64
        | NamedRecordTypeNotFound of string
        | RecordNotFoundByIDWithName of int * string
        | NoRecordsWereFound
        | TooManyRecordsFoundx
        | RecordNotFoundByIDString of string
        | RecordNotFoundByIDStringWithName of string * string
        | RecordAlreadyExists of string
        | RecordCouldNotBeSaved of string seq
        | RecordCouldNotBeDeleted of string seq
        | InvalidDecimalValue of string
        | InvalidIntValue of string
        | UnexepectedNullValue of string
        | InvalidOperation of string
        | InvalidValue of string
        | InvalidDateValue of string
        | FailedToSaveChangesToDatabase
        | FailedToSaveChangesToDatabaseWithDetails of string
        | FailedToParseValue of string
        | InvalidIndex of int
        | RequriredValueNotFound of string
        | KeyValueNotFound of string

        static member StringifyErrors x = x |+ StandardErrors.StringifyError |> ToList
        static member StringifyError x =
            x
            |> function
            | AggregateError x -> x |> StandardErrors.StringifyErrors |> JoinPeriodSpace <-< "Aggregate Error"
            | KeyValueNotFound x -> "Key Value Not Found" >-> x |> str
            | UnexepectedNullValue x -> "UnexepectedNullValue" >-> x
            | AnUnexpectedErrorOccurredDetailed (msg, ex) -> ex |> TheNewBestExceptionToStringFull <-< msg
            | RequriredValueNotFound descr -> descr <-< "A required value was not found"
            | FailedToGetRecords ex -> ex |> ToString <-< "Failed to Get Records"
            | NestedErrors errs -> System.String.Join("\r\n", errs)
            | AnUnexpectedErrorOccurredAsMessage errs -> errs |> Label "Unexpected Error"
            | NamedRecordTypeNotFound name -> sprintf "The %s could not be found" name
            | RecordNotFoundByIDWithName (id, name) -> sprintf "%s not found for ID %i" name id
            | RecordNotFoundByIDStringWithName (id, name) -> sprintf "%s not found for ID %s" name id
            | NoRecordsWereFound -> "No records were found"
            | InvalidOperation x -> "Invalid Operation" >-> x
            | FailedToParseValue x -> x |> EmptyToDefault "[Empty]" <-< "Failed to Parse Value"
            | TooManyRecordsFoundx -> "Too many records found"
            | AnUnexpectedErrorOccurred ex -> "An Unexpected Error Occurred: " + ex.ToString()
            | FailedToSaveChangesToDatabase -> "Failed to save changes to the database"
            | FailedToSaveChangesToDatabaseWithDetails x -> "Failed to save changes to the database" >-> x
            | InvalidUserProfileID x -> "Invalid User Profile ID" >-> x
            | RecordAlreadyExists thing -> "A record already exists for this " + thing
            | RecordCouldNotBeSaved errors -> "The record could not be saved" >-> (errors |> String.concat " | ")
            | RecordCouldNotBeDeleted errors -> "The record could not be deleted" >-> (errors |> String.concat " | ")
            | InvalidDecimalValue str -> "Invalid Decimal Value" >-> str
            | InvalidDateValue str -> "Invalid Date Value" >-> str
            | InvalidIntValue str -> "Invalid Integer Value" >-> str
            | InvalidValue str -> "Invalid Value" >-> str
            | RecordNotFoundByID id -> sprintf "Record not found for ID %i" id
            | InvalidIndex ix -> sprintf "Invalid IndexID %i" ix
            | RecordNotFoundByID64 id -> sprintf "Record not found for ID %i" id
            | RecordNotFoundByIDString id -> sprintf "Record not found for ID %s" id
