-- MATHFUN
-- UP811216

import Control.DeepSeq
import Control.Exception
import Data.List
import Data.Typeable
import Data.Char (isSpace, isDigit, digitToInt)
type Title = String
type Director = String
type Year = Int
type Likes = [String]
type Dislikes = [String]

type Film = (Title, Director, Year, Likes, Dislikes)

testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"],["Sam", "Olga", "Tim"]), ("The Fly","David Cronenberg", 1986, ["Garry", "Dave", "Zoe"], ["Kevin", "Emma", "Heidi", "Jo", "Kate"]), ("Body Of Lies","Ridley Scott", 2008, ["Garry", "Dave"], ["Bill", "Olga", "Tim", "Zoe", "Paula"]), ("Avatar", "James Cameron", 2009,["Dave", "Amy", "Liz"], ["Olga", "Tim", "Zoe", "Paula"]), ("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"], ["Sam", "Wally", "Kate"]), ("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"], ["Olga", "Dave", "Kate", "Zoe"]), ("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"], ["Tim", "Emma", "Jo", "Olga"]), ("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"], ["Tim", "Garry", "Ian", "Neal"]), ("Alien: Covenant", "Ridley Scott", 2017, ["Kevin", "Tim"], ["Emma", "Jo", "Liz"]), ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"], ["Jenny", "Kate", "Emma", "Olga"]), ("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"], ["Bill", "Garry", "Ian", "Kate"]), ("Jaws", "Steven Spielberg", 1975, ["Jenny", "Emma", "Bill", "Neal"], ["Sam", "Ian", "Kate"]), ("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"], ["Ian", "Neal", "Tim", "Liz"]), ("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"], ["Neal"]), ("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"], ["Jo"]), ("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Garry"], ["Heidi", "Bill", "Sam", "Zoe"]), ("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"], ["Kate", "Jenny", "Zoe"]), ("True Lies", "James Cameron", 1994, ["Sam", "Dave"], ["Emma", "Olga", "Jenny", "Zoe"]), ("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"], ["Wally", "Dave", "Jenny", "Zoe"]),  ("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"], ["Olga", "Heidi"]), ("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"], ["Heidi", "Jenny", "Sam"]), ("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"], ["Dave", "Olga"]), ("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"], ["Heidi"]), ("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"], ["Olga", "Jo", "Neal"]), ("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"], ["Kate", "Bill", "Dave"])]
addFilm :: Title -> Director -> Year -> [Film]
addFilm a b c  = updateFilmDatabase (a, b, c, [], [])  testDatabase
giveAllFilms :: [Film] ->  String
giveAllFilms testDatabase = (concat (intersperse ", " [ filmAsString i | i<-testDatabase]))
checkDirector :: Film -> Director ->  Bool
checkDirector (_, a, _, _, _) director = director == a
giveFilmsByDirector :: Director -> [Film] -> [Film]
giveFilmsByDirector director database = [ i | i<-database,  checkDirector i director]
giveFilmsByDirectorString :: Director ->  [Film] -> String
giveFilmsByDirectorString director database =  (concat (intersperse ", " [ filmAsString i | i<-database,  checkDirector i director]))
filmRatingCheck :: Film -> Float ->   Bool
filmRatingCheck (_, _, _, d, e)  limit = (fromIntegral (length d) / fromIntegral (length d +  length e)) >= limit
findRating :: [String] -> [String] ->  Float
findRating a b = fromIntegral (length a) / fromIntegral (length a +  length b)
floatFormat :: Float -> Float
floatFormat a =  (fromInteger $ round $ a * (10^1)) / (10.0^^1) *100
doubleFormat :: Double -> Double
doubleFormat a =  (fromInteger $ round $ a * (10^1)) / (10.0^^1)
giveFilmsByLimit :: Float ->  [Film] -> String
giveFilmsByLimit limit database =   (concat (intersperse ", " [  filmAsString i | i<-database,  filmRatingCheck i limit]))
convertToFloat :: Film -> Float
convertToFloat (_, _, _, d, e) = (fromIntegral (length d) / fromIntegral (length d +  length e))
directorRatings :: [Film]-> [Float]
directorRatings directorRatingsList =  [  convertToFloat i | i<-directorRatingsList ]
addList :: [Float] -> Float
addList []     = 0
addList (x:xs) = x + addList xs
averageRatingDirector ::  Director -> [Film] -> String
averageRatingDirector director database =  "Website Rating: " ++ show  (floatFormat((addList (directorRatings (giveFilmsByDirector director database))) / fromIntegral (length (directorRatings (giveFilmsByDirector director database)))))
checkLikes :: Likes -> Dislikes -> String ->  Bool
checkLikes likes dislikes user
                          | (checkInLike likes user == True) || (checkInDislike dislikes user) == True = True
                          | otherwise = False
checkUser :: Film -> String -> Bool
checkUser (_, _, _, d, e) user = checkLikes d e user
checkInLike :: Likes ->  String -> Bool
checkInLike likes user
                    | likes == [] = False
                    | (head likes) /= user = checkInLike (tail likes)  user
                    | otherwise = True
checkInDislike :: Dislikes ->  String -> Bool
checkInDislike dislikes user
                    | dislikes == [] = False
                    | (head dislikes) /= user = checkInDislike (tail dislikes)  user
                    | otherwise = True
rating :: Likes -> String -> String
rating d user
          | (checkInLike d user) == True = "Liked"
          | otherwise = "Disliked"
stringConvert :: Film -> String -> String
stringConvert (a, _, _, d, _) user = "Title: " ++  a ++ " Status: " ++ (rating d user)
filmAsString :: Film -> String
filmAsString (a, b, c, d, e) = "\n\nTitle: " ++ a ++ " \nDirector: " ++  b ++ " \nYear: " ++  show c ++ " \nWebsite Rating: " ++  show  (round(floatFormat ((findRating d e)) )) ++ "%"
userFilms :: String->  [Film] -> String
userFilms user  database=  (concat (intersperse ", " [  stringConvert i user  | i<-database, checkUser i user]))
checkTitleNot :: Film -> Title ->  Bool
checkTitleNot (a, _, _, _, _) title = title /= a
checkTitleEqual :: Film -> Title ->  Bool
checkTitleEqual (a, _, _, _, _) title = title == a
getFilm :: Title -> [Film] -> Film
getFilm title testDatabase
            | testDatabase == [] =  ("Na", "Na", 0, [], [])
            | ((checkTitleEqual   (head testDatabase) title) == False) = getFilm title (tail testDatabase)
            | otherwise =  (head testDatabase)
rate :: Film -> String -> String -> Film
rate (a, b, c, d, e) user pref
                    | pref == "like" = (a, b, c, user:d, e)
                    | otherwise = (a, b, c, d, user:e)
updateFilmDatabase :: Film -> [Film] -> [Film]
updateFilmDatabase (a, b, c, d, e)  testDatabase =  [i | i<-testDatabase, checkTitleNot i a] ++ [(a, b, c, d, e)]
likeFilm :: Film -> String -> Film
likeFilm (a, b,c,d,e) user
                      | checkInDislike e user = (a, b, c, [user] ++ d, filter (/= user) e)
                      | checkInLike d user  = (a, b, c, d, e)
                      | otherwise = rate (a, b, c, d, e) user "like"
disLikeFilm :: Film -> String -> Film
disLikeFilm (a, b,c,d,e) user
                          | checkInLike d user = (a, b, c, filter (/= user) d, [user] ++ e)
                          | checkInDislike e user  = (a, b, c, d, e)
                          | otherwise = rate (a, b, c, d, e) user "dislike"
checkUserChange :: Title -> [Film] -> String -> String -> [Film]
checkUserChange title testDatabase user pref
                                  |  pref == "like" =   (updateFilmDatabase (likeFilm (getFilm title testDatabase) user) testDatabase)
                                  |  pref == "dislike" =   ( updateFilmDatabase (disLikeFilm (getFilm title testDatabase) user) testDatabase)
                                  |  otherwise =   ( updateFilmDatabase (rate (getFilm title testDatabase) user pref) testDatabase)


checkFilmByYears :: Film -> Year -> Year ->  Bool
checkFilmByYears  (_,_,a,_,_) startYear endYear
                                            | a>=startYear && a <= endYear = True
                                            | otherwise = False
quicksort :: [Film] -> [Film]
quicksort [] = []
quicksort ((a, b,c, d, e):xs) =
    let smallerSorted = quicksort [ (title, director, year, likes, dislikes)| (title, director, year, likes, dislikes) <- xs,  findRating likes dislikes <= findRating d e]
        biggerSorted = quicksort [ (title, director, year, likes, dislikes) | (title, director, year, likes, dislikes) <- xs,  findRating likes dislikes >= findRating d e]
    in   biggerSorted ++ [(a, b,c, d, e)] ++  smallerSorted
giveFilmsByYear :: Year -> Year -> [Film] -> [Film]
giveFilmsByYear startYear endYear  database=  quicksort [ i|  i<-database, checkFilmByYears i startYear endYear]

giveFilmsByYearString :: Year -> Year -> [Film] -> String

giveFilmsByYearString startYear endYear  database =  (concat (intersperse ", " [ filmAsString i |  i<-(giveFilmsByYear startYear endYear database)] ))

filmsAsStringConvertFile :: Film -> String
filmsAsStringConvertFile (a, b, c, d, e) = "(" ++  show a ++ "," ++  show b ++ "," ++ (show c) ++ ", " ++ "["++ (concat (intersperse ", " [ i |  i<-d] )) ++ "]" ++ "," ++ "["++ (concat (intersperse ", " [ i |  i<-e] )) ++ "]"
convertToFile :: [Film] -> String
convertToFile database =   (concat (intersperse ", " [ filmsAsStringConvertFile i |  i<-database] )) ++ ""

isNumber :: String -> Bool
isNumber ""  = False
isNumber "." = False
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False


checkValidNumber :: String -> Bool
checkValidNumber  x
                  | ((round(read x :: Float) <=9) == True)  && ((round(read x :: Float) >=1) ==True)  = True
                  | otherwise = False

getInt :: String -> Int
getInt  x = (round(read x :: Float))

---IO---
loadDatabase :: IO [Film]
loadDatabase = do
  database <- readFile "films.txt"
  evaluate (force database)
  return  (read database)


drawMenu :: IO ()
drawMenu = do
          putStrLn "Main Menu"
          putStrLn "Option 1: Add a new Film to the database"
          putStrLn "Option 2: Give all of the films of the database"
          putStrLn "Option 3: Give all films by a given director"
          putStrLn "Option 4: Give all films that have a rating of 75% or higher (Actually limit) "
          putStrLn "Option 5: Give the average website rating for the films of a given director"
          putStrLn "Option 6: Give the titles of all films that a particular user has rated, along with how they have been rated (‘like’ or ‘dislike’) by that user"
          putStrLn "Option 7: Allow a user to say they like or dislike a particular film"
          putStrLn "Option 8: Give all the films released between two given years (inclusive), sorted in descending order of website rating"
          putStrLn "Option 9: Exit program"
          putStrLn ""



addFilmIO :: [Film] -> IO [Film]
addFilmIO database = do
                putStrLn "Enter a title:"
                title <- getLine
                if (isNumber title) ==False
                then do
                    putStrLn "Enter a director:"
                    director <- getLine
                    if (isNumber director) ==False
                    then do
                          putStrLn "Enter a year:"
                          year <- getLine
                          if (isNumber year) ==True
                          then do
                                  return (addFilm title director (getInt year))
                          else do
                                  putStrLn "Year cannot be a a word"
                                  addFilmIO database
                    else do
                            putStrLn "Director cannot be a number"
                            addFilmIO database
                else do
                        putStrLn "Title cannot be a number"
                        addFilmIO database


giveFilmsDirectorIO :: [Film] -> IO String
giveFilmsDirectorIO database = do
                    putStrLn "Enter a director:"
                    director <- getLine
                    if (isNumber director) ==False
                    then do
                          return (giveFilmsByDirectorString director database)
                    else do
                          putStrLn "Title must be a word, no numbers are allowed"
                          giveFilmsDirectorIO database


giveFilmsByLimitIO :: [Film] ->  IO String
giveFilmsByLimitIO database = do
                          putStrLn "Enter a a limit between 0 and 1:"
                          limit <- getLine
                          if ( isNumber limit) == True
                          then do
                            return (giveFilmsByLimit (read limit :: Float) database)
                          else do
                            putStrLn "Limit must be a number, no words are allowed"
                            giveFilmsByLimitIO database



giveAverageDirectorIO :: [Film] ->  IO String
giveAverageDirectorIO database = do
                    putStrLn "Enter a director:"
                    director <- getLine
                    if (isNumber director) ==False
                    then do
                          return (averageRatingDirector director database)
                    else do
                          putStrLn "Title must be a word, no numbers are allowed"
                          giveFilmsDirectorIO database


giveFilmsUserRatedIO :: [Film] ->  IO String
giveFilmsUserRatedIO database = do
                    putStrLn "Enter a user:"
                    user <- getLine
                    if (isNumber user) ==False
                    then do
                          return (userFilms user database)
                    else do
                          putStrLn "User must be a word, no numbers are allowed"
                          giveFilmsDirectorIO database

changeMind :: [Film] ->  IO [Film]
changeMind database = do
                    putStrLn "Enter a title:"
                    title <- getLine
                    if (isNumber title) ==False
                    then do
                          putStrLn "Enter a user:"
                          user <- getLine
                          if (isNumber user) ==False
                          then do
                                putStrLn "Enter a your preference like or dislike:"
                                pref <- getLine
                                if (isNumber pref) ==False
                                then do
                                      return (checkUserChange title database user pref)
                                else do
                                      putStrLn "title must be a word, no numbers are allowed"
                                      changeMind database

                          else do
                                putStrLn "user must be a word, no numbers are allowed"
                                changeMind database

                    else do
                          putStrLn "title must be a word, no numbers are allowed"
                          changeMind database



giveFilmsByTwoYearsIO :: [Film] ->  IO String
giveFilmsByTwoYearsIO database = do
                    putStrLn "Enter a start year:"
                    startYear <- getLine
                    if (isNumber startYear) ==True
                    then do
                          putStrLn "Enter a  end year:"
                          endYear <- getLine
                          if (isNumber endYear) ==True
                          then do
                                return (giveFilmsByYearString (getInt(startYear)) (getInt(endYear)) database)
                          else do
                                putStrLn "The start year must be a number, no words are allowed"
                                giveFilmsByTwoYearsIO database
                    else do
                          putStrLn "The start year must be a number, no words are allowed"
                          giveFilmsByTwoYearsIO database



selectOptions :: String -> [Film] ->IO ()
selectOptions  user database =  do
              putStrLn "Select your option:"
              getOptionNumber <- getLine
              if (isNumber getOptionNumber == True)
              then do
                    if (checkValidNumber getOptionNumber) ==True
                    then do
                           case (getInt getOptionNumber) of
                            1 -> do
                                newDatabase <- addFilmIO database
                                drawMenu
                                selectOptions user newDatabase
                                putStrLn ""
                            2 -> do
                                putStrLn (giveAllFilms database)
                                drawMenu
                                selectOptions user database
                                putStrLn ""
                            3 -> do
                                 films <- giveFilmsDirectorIO database
                                 putStrLn films
                                 drawMenu
                                 selectOptions user database
                                 putStrLn ""
                            4 -> do
                                  films <- giveFilmsByLimitIO database
                                  putStrLn films
                                  drawMenu
                                  selectOptions user database
                                  putStrLn ""
                            5 -> do
                                  averagRatingString <- giveAverageDirectorIO database
                                  putStrLn averagRatingString
                                  drawMenu
                                  selectOptions user database
                                  putStrLn ""
                            6 -> do
                                  films <- giveFilmsUserRatedIO database
                                  putStrLn films
                                  drawMenu
                                  selectOptions user database
                                  putStrLn ""
                            7 -> do
                                  newDatabase <- changeMind database
                                  drawMenu
                                  selectOptions user newDatabase
                                  putStrLn ""
                            8 -> do
                                  films <- giveFilmsByTwoYearsIO database
                                  putStrLn films
                                  drawMenu
                                  selectOptions user database
                                  putStrLn ""
                            9 -> do
                                writeFile "films.txt"   (convertToFile (database))
                                return ()
                    else do
                    putStrLn "This number is out of range, it must be between 1 and 9"
                    selectOptions user database
              else do
                    putStrLn "We require a number from 1 to 9, we cannot accept words and letters or special charactors"
                    selectOptions user database

main :: IO ()

main = do
      database <- loadDatabase
      putStrLn "Enter your name:"
      user<- getLine
      if  (isNumber user == False)
      then do
             if (length user >0)
             then do
                  putStrLn (giveAllFilms database)
                  drawMenu
                  selectOptions user database

             else do
                putStrLn "A name cannot be empty"
                main
      else do
                  putStrLn "You cannot enter a number here, we require a name to continue"
                  main
