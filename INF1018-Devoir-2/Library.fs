namespace INF1018_Devoir_2

open System
open System.Collections.Generic

(* 
Construisez un automate d'analyse lexicale pour cet alphabet
d'un langage d'arithmétique simple :

<var> := A | B | C
<int> := Séquence de chiffre représentant un entier valide
<op> := + | - | * | /

Basez votre automate sur l'approche procédurale 
basée sur les diagrammes d'états-transitions.
*)

module Lex =
    type Var = A | B | C
    type Op = Plus | Minus | Multiply | Divide
    type LexicalUnit = Var of Var | Int of int | Op of Op
    
    type IndexedString = { string: string; mutable index : int; output: List<LexicalUnit> }

    let peekChar { string = string; index = index } = 
        if index < string.Length
        then Some string.[index]
        else None

    let incIndex istring =
        istring.index <- istring.index + 1
        
    let popChar istring =
        let c = peekChar istring
        incIndex istring
        c

    let skipWhitespace istring =
        while peekChar istring = Some ' ' do incIndex istring

    // Prend une chaîne de caractère en entrée et retourne une liste de lexèmes
    // si la chaîne en entrée est une expression de la grammaire valide.
    let parse string =
        let istring = { string = string; index = 0; output = List<LexicalUnit>() }

        // Votre code ici.

        while peekChar istring <> None do
            skipWhitespace istring

            match peekChar istring with
            | Some 'A' ->
                istring.output.Add(Var A)
                incIndex istring

            | Some 'B' ->
                istring.output.Add(Var B)
                incIndex istring

            | Some 'C' ->
                istring.output.Add(Var C)
                incIndex istring

            | Some '+' ->
                istring.output.Add(Op Plus)
                incIndex istring

            | Some '-' ->
                istring.output.Add(Op Minus)
                incIndex istring

            | Some '*' ->
                istring.output.Add(Op Multiply)
                incIndex istring

            | Some '/' ->
                istring.output.Add(Op Divide)
                incIndex istring

            | Some c when Char.IsDigit(c) ->
                let mutable nombre = ""

                while (peekChar istring |> Option.exists Char.IsDigit) do
                    match popChar istring with
                    | Some chiffre ->
                        nombre <- nombre + string chiffre
                    | None -> ()

                match Int32.TryParse(nombre) with
                | true, valeur ->
                    istring.output.Add(Int valeur)
                | false, _ ->
                    failwith "Entier invalide"

            | Some _ ->
                failwith "Caractere invalide"

            | None -> ()

        istring.output |> List.ofSeq
