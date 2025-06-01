// --------------------------------------------------------------
// ParseLine and ParseInput
// Zarak Khan
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists. Each
// sub-list denotes one bike ride.
// 
// Example:
//   [ [21864678; 6035; 3000; 23; 317; 338; 1; 1; 1974]; ... ]
//
// The values are:
//    trip_id
//    bike_id
//    trip_duration   (seconds)
//    starting_hour   (between 0 and 23)
//    from_station_id
//    to_station_id
//    is_subscriber   (1 if yes, 0 if no)
//    gender          (0 if not specified, 1 if identifies as male, 2 if identifies as female)
//    birth_year      (0 if not specified)
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  // Map each line to a list of integers, then convert the sequence to a list.
  lines |> Seq.map ParseLine |> Seq.toList



// --------------------------------------------------------------
// Helper Functions
//

/// Computes the percentage given a count and total.
/// Returns 0.0 if total is 0.
let percentage count total =
    if total = 0 then 0.0
    else (float count / float total) * 100.0

/// Converts the ride's duration from seconds (field index 2) to minutes.
let rideDurationInMinutes ride =
    (float (List.item 2 ride)) / 60.0

/// Computes the average age of riders from the ride data.
/// Only rides with a valid birth year (non-zero) are included.
/// Uses System.DateTime.Now.Year to calculate the age.
let averageAge rides =
    let currentYear = System.DateTime.Now.Year
    // Filter rides with a valid birth year.
    let validRides = rides |> List.filter (fun ride -> List.item 8 ride <> 0)
    if List.isEmpty validRides then 0.0
    else 
      // Sum the ages of all valid rides and divide by the number of valid rides.
      let totalAge = validRides |> List.sumBy (fun ride -> currentYear - List.item 8 ride)
      float totalAge / float (List.length validRides)

/// Counts the number of rides that satisfy the given predicate.
let countRides rides predicate =
    rides |> List.filter predicate |> List.length

/// Creates a formatted histogram line for a given hour.
/// The number of asterisks is the count of rides for that hour divided by 100.
let histogramLine hour rides =
    let count = countRides rides (fun ride -> List.item 3 ride = hour)
    // Each asterisk represents 100 rides.
    let stars = String.replicate (count / 100) "*"
    sprintf "   %d: %s%d" hour stars count


// --------------------------------------------------------------
// Main function
// 
[<EntryPoint>]
let main argv =

  // Print header information.
  printfn "Project 3: Divvy Rides Data Analysis with F#"
  printfn "CS 341, Spring 2025"
  printfn ""
  printfn "This application allows you to analyze and visualize"
  printfn "information about Divvy bike rides in Chicago, such as"
  printfn "the number of male/female riders, the average age, etc."
  printfn ""

  // Prompt the user for the name of the file containing the ride data.
  printf "Enter the name of the file with the Divvy ride data: "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents

  // Calculate the total number of trips.
  let totalTrips = List.length ridedata

  // Calculate the number of rides by gender.
  // Gender is the 8th value in each ride record (index 7):
  // 1 for male, 2 for female.
  let maleCount = countRides ridedata (fun ride -> List.item 7 ride = 1)
  let femaleCount = countRides ridedata (fun ride -> List.item 7 ride = 2)
  
  // Calculate percentages for male and female riders.
  let malePerc = percentage maleCount totalTrips
  let femalePerc = percentage femaleCount totalTrips

  // Calculate the average age of riders.
  let avgAge = averageAge ridedata

  // Calculate ride durations by category.
  // Note: Convert trip duration from seconds to minutes before comparison.
  // We only count rides with a positive duration.
  //   0-30 mins: duration (in minutes) > 0 and <= 30.0
  //   30-60 mins: duration > 30.0 and <= 60.0
  //   60-120 mins: duration > 60.0 and <= 120.0
  //   > 2 hours: duration > 120.0
  let duration0_30 = countRides ridedata (fun ride -> 
      let d = rideDurationInMinutes ride 
      d > 0.0 && d <= 30.0)
  let duration30_60 = countRides ridedata (fun ride -> 
      let d = rideDurationInMinutes ride 
      d > 30.0 && d <= 60.0)
  let duration60_120 = countRides ridedata (fun ride -> 
      let d = rideDurationInMinutes ride 
      d > 60.0 && d <= 120.0)
  let durationOver2 = countRides ridedata (fun ride -> 
      let d = rideDurationInMinutes ride 
      d > 120.0)

  // Calculate percentages for ride durations based on the total number of trips.
  let durPerc0_30 = percentage duration0_30 totalTrips
  let durPerc30_60 = percentage duration30_60 totalTrips
  let durPerc60_120 = percentage duration60_120 totalTrips
  let durPercOver2 = percentage durationOver2 totalTrips

  // Print analysis results.
  printfn ""
  printfn "Number of Trips: %d" totalTrips
  printfn ""
  
  // Append a literal "%" sign after %A for the percentages.
  printfn "Number of Riders Identifying as Male: %d (%A%%)" maleCount malePerc
  printfn "Number of Riders Identifying as Female: %d (%A%%)" femaleCount femalePerc
  printfn ""
  
  printfn "Average Age: %A" avgAge
  printfn ""
  
  printfn "Ride Durations: "
  printfn "   0-30 mins: %d (%A%%)" duration0_30 durPerc0_30
  printfn "   30-60 mins: %d (%A%%)" duration30_60 durPerc30_60
  printfn "   60-120 mins: %d (%A%%)" duration60_120 durPerc60_120
  printfn "   > 2 hours: %d (%A%%)" durationOver2 durPercOver2
  printfn ""
  
  // Print the histogram of start times for hours 0 to 23.
  printfn "Histogram of Start Times:"
  [0..23]
  |> List.iter (fun hour -> printfn "%s" (histogramLine hour ridedata))
  
  printfn ""
  printfn "Exiting program."
  0
