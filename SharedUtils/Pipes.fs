namespace SharedUtils
    
open System

module Pipes = 
    type StringPipe = 
        static member JoinLines(text: string seq): string = String.Join("\r\n", text)
