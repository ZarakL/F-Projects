//
// F# program to analyze and transform a phrase.
// Offers operations: length, vowel counts, character counts, palindrome check, and vowel rotation.
//
// Name: Zarak Khan


(*


Test Case 1:
Input: "F# is fun!"
Purpose:
  - Tests general functionality with mixed characters (letters, punctuation, and spaces).
  - Verifies that the length is computed correctly, vowels (only lowercase) are counted properly,
    character counts letters (a–z) and punctuation are accurate, and that vowel rotation is applied.
Output (when selecting option 7):
  Exploded: ['F'; '#'; ' '; 'i'; 's'; ' '; 'f'; 'u'; 'n'; '!']
  Length of phrase: 10
  Number of vowels: 2
  Number of each vowel in the phrase:
    'a': 0
    'e': 0
    'i': 1
    'o': 0
    'u': 1
  Number of each character in the phrase: (counts lowercase letters and punctuation; note uppercase letters aren’t counted)
  The phrase cannot be rearranged into a palindrome.
  Rotated vowels: "F# os fan!"   // 'i' -> 'o', 'u' -> 'a'

Test Case 2:
Input: "racecar"
Purpose:
  - Tests a known palindrome phrase.
  - Verifies proper counting of vowels and characters, and that the palindrome check returns true.
  - Checks that vowel rotation changes only the vowels.
Output (when selecting option 7):
  Exploded: ['r'; 'a'; 'c'; 'e'; 'c'; 'a'; 'r']
  Length of phrase: 7
  Number of vowels: 2
  Number of each vowel in the phrase:
    'a': 2
    'e': 1
    'i': 0
    'o': 0
    'u': 0
  Number of each character in the phrase: (counts letters a–z and punctuation)
  The phrase can be rearranged into a palindrome!
  Rotated vowels: "recicer"   // 'a' -> 'e', 'e' -> 'i'

Test Case 3:
Input: ""
Purpose:
  - Tests the edge case of an empty string.
  - Verifies that functions handle empty input without error (length 0, no vowels, and proper counts).
Output (when selecting option 7):
  Exploded: []
  Length of phrase: 0
  Number of vowels: 0
  Number of each vowel in the phrase:
    'a': 0
    'e': 0
    'i': 0
    'o': 0
    'u': 0
  Number of each character in the phrase: (all counts 0)
  The phrase can be rearranged into a palindrome!  // An empty string is considered a palindrome.
  Rotated vowels: ""  // Empty string remains empty.

Test Case 4:
Input: "aAaEeIiOoUu"
Purpose:
  - Tests that only lowercase vowels are counted and rotated.
  - Verifies that uppercase vowels are ignored in vowel-related functions.
Output (when selecting option 7):
  Exploded: ['a'; 'A'; 'a'; 'E'; 'e'; 'I'; 'i'; 'O'; 'o'; 'U'; 'u']
  Length of phrase: 11
  Number of vowels: 6   // Only positions with lowercase vowels ('a', 'a', 'e', 'i', 'o', 'u') are counted.
  Number of each vowel in the phrase:
    'a': 2
    'e': 1
    'i': 1
    'o': 1
    'u': 1
  Number of each character in the phrase: (only lowercase letters and punctuation are counted; uppercase count as 0)
  The phrase cannot be rearranged into a palindrome!
  Rotated vowels: "eAeEiIoOuUa"  
    // Only the lowercase vowels are rotated: 'a'->'e', 'a'->'e', 'e'->'i', 'i'->'o', 'o'->'u', 'u'->'a'
*)

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple"  -> ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e']  -> "the"
//
let implode (L:char list) = 
  new string(List.toArray L)

//
// Custom functions
//

// Recursively compute the length of a list
let rec myLength lst =
  match lst with
  | [] -> 0
  | _::xs -> 1 + myLength xs

// Check if a character is a vowel (only lowercase considered)
let isVowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

// Count occurrences of a specific character in a list
let countChar target lst =
  List.fold (fun acc c -> if c = target then acc + 1 else acc) 0 lst

// Count total number of vowels in the list
let countVowels lst =
  List.fold (fun acc c -> if isVowel c then acc + 1 else acc) 0 lst

// Count each vowel: returns a list of tuples (vowel, count)
let countEachVowel lst =
  let vowels = ['a'; 'e'; 'i'; 'o'; 'u']
  List.map (fun v -> (v, countChar v lst)) vowels

// Count each character letters a–z and the specified punctuation marks.
let countCharacters lst =
  let letters = ['a' .. 'z']
  let punctuation = ['.'; '?'; '!'; ','; ';'; ':'; '-'; '('; ')'; '['; ']'; '{'; '}']
  let letterCounts = List.map (fun c -> (c, countChar c lst)) letters
  let punctuationCounts = List.map (fun c -> (c, countChar c lst)) punctuation
  letterCounts @ punctuationCounts

// Check if the phrase can be rearranged into a palindrome.
// Only consider letters a–z.
let canFormPalindrome lst =
  // Filter only letters a–z
  let lettersOnly = List.filter (fun c -> c >= 'a' && c <= 'z') lst
  // each letter a–z, count occurrences in the filtered list.
  let counts = List.map (fun c -> countChar c lettersOnly) ['a' .. 'z']
  // Count how many letters have an odd count.
  let oddCount = List.fold (fun acc count -> if count % 2 = 1 then acc + 1 else acc) 0 counts
  oddCount <= 1

// Rotate vowels: a -> e, e -> i, i -> o, o -> u, u -> a.
let rotateVowel c =
  match c with
  | 'a' -> 'e'
  | 'e' -> 'i'
  | 'i' -> 'o'
  | 'o' -> 'u'
  | 'u' -> 'a'
  | _ -> c

// Apply vowel rotation to the entire list of characters.
let rotateVowels lst =
  List.map rotateVowel lst

//
// Main entry point
//
[<EntryPoint>]
let main argv =
  printfn "Starting..."
  printfn ""

  // Read in the string from user input
  printf "Enter your input> "
  let input = System.Console.ReadLine()
  printfn ""

  // Turn the string into a list of characters
  let L = explode input
  printfn "Exploded: %A" L
  printfn ""

  // Print the options, and read in the selection from user input
  printfn "Select a command:"
  printfn "  1. Determine the length of the phrase"
  printfn "  2. Find the number of vowels in the phrase"
  printfn "  3. Calculate the number of each vowel in the phrase"
  printfn "  4. Calculate the number of each character in the phrase"
  printfn "  5. Check if the phrase can be rearranged into a palindrome"
  printfn "  6. Rotate the vowels in the phrase"
  printfn "  7. All of the above!"
  printf "Your choice> "
  let cmd = int(System.Console.ReadLine())
  printfn ""

  // Functions to execute each command
  let option1 () =
    let len = myLength L
    printfn "Length of phrase: %A" len

  let option2 () =
    let numVowels = countVowels L
    printfn "Number of vowels: %A" numVowels

  let option3 () =
    let vowelCounts = countEachVowel L
    printfn "Number of each vowel in the phrase:"
    List.iter (fun (v, count) -> printfn "'%c': %A" v count) vowelCounts

  let option4 () =
    let charCounts = countCharacters L
    printfn "Number of each character in the phrase:"
    List.iter (fun (c, count) -> printfn "'%c': %A" c count) charCounts

  let option5 () =
    if canFormPalindrome L then
      printfn "The phrase can be rearranged into a palindrome!"
    else
      printfn "The phrase cannot be rearranged into a palindrome."

  let option6 () =
    let rotatedList = rotateVowels L
    let rotatedString = implode rotatedList
    // Print the rotated vowels string enclosed in quotes as shown in the sample output.
    printfn "Rotated vowels: \"%s\"" rotatedString

  // Execute the selected command
  match cmd with
  | 1 -> option1 ()
  | 2 -> option2 ()
  | 3 -> option3 ()
  | 4 -> option4 ()
  | 5 -> option5 ()
  | 6 -> option6 ()
  | 7 ->
      option1 ()
      printfn ""
      option2 ()
      printfn ""
      option3 ()
      printfn ""
      option4 ()
      printfn ""
      option5 ()
      printfn ""
      option6 ()
  | _ -> printfn "Command not recognized."

  printfn "Done!"
  0  // return 0  -> success, much like C++
