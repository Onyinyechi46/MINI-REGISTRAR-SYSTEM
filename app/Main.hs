-- Mini Registrar System
-- Demonstrates Applicatives, Functors, Monoids, and Monads in a practical registrar

module Main where

import System.IO (hFlush, stdout)
import Control.Applicative

-- ==============================
-- Data Types
-- ==============================

-- Score wrapper
newtype Score a = Score { getScore :: Maybe a }
  deriving (Show, Eq)

-- Student type
data Student = Student
  { studentName  :: String
  , studentScore :: Score Int
  } deriving (Show, Eq)

-- ==============================
-- Instances
-- ==============================

instance Functor Score where
  fmap f (Score (Just x)) = Score (Just (f x))
  fmap _ (Score Nothing)  = Score Nothing

instance Applicative Score where
  pure x = Score (Just x)
  (Score (Just f)) <*> (Score (Just x)) = Score (Just (f x))
  _ <*> _ = Score Nothing

instance Monad Score where
  (Score (Just x)) >>= f = f x
  _ >>= _ = Score Nothing

-- ✅ Single, correct Monoid & Semigroup instance
instance Num a => Semigroup (Score a) where
  (Score (Just x)) <> (Score (Just y)) = Score (Just (x + y))
  (Score (Just x)) <> _                = Score (Just x)
  _ <> (Score (Just y))                = Score (Just y)
  _ <> _                               = Score Nothing

instance Num a => Monoid (Score a) where
  mempty = Score (Just 0)

-- ==============================
-- Utilities
-- ==============================

pause :: IO ()
pause = do
  putStrLn "\nPress ENTER to return to the main menu..."
  _ <- getLine
  return ()

safeReadInt :: String -> Maybe Int
safeReadInt s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-- ==============================
-- Registrar Functions
-- ==============================

-- 1. Register a student
registerStudent :: IO Student
registerStudent = do
  putStrLn "\n--- Register New Student ---"
  putStr "Enter name: "
  hFlush stdout
  name <- getLine

  putStr "Enter grade (0-100): "
  hFlush stdout
  gradeStr <- getLine

  case safeReadInt gradeStr of
    Just g -> do
      let student = Student name (pure g)  -- Applicative (pure)
      putStrLn "\nStudent registered successfully!"
      putStrLn $ "Name: " ++ studentName student
      putStrLn $ "Grade: " ++ show (getScore (studentScore student))
      pause
      return student
    Nothing -> do
      putStrLn "Invalid grade input. Try again."
      registerStudent

-- 2. Show Student Info
showStudent :: Student -> IO ()
showStudent s = do
  putStrLn "\n--- Student Information ---"
  putStrLn $ "Name:  " ++ studentName s
  putStrLn $ "Grade: " ++ show (getScore (studentScore s))
  putStrLn "Student info displayed successfully."
  pause

-- 3. Compute Class Total
computeClassTotal :: [Student] -> IO ()
computeClassTotal students = do
  let total = mconcat [studentScore s | s <- students] -- Monoid in action
  putStrLn "\n--- Class Total ---"
  putStrLn $ "Total of all grades: " ++ show (getScore total)
  putStrLn "Computed class total successfully."
  pause

-- 4. Check Grade Category
checkGradeCategory :: Student -> IO ()
checkGradeCategory (Student name (Score (Just g)))
  | g >= 70   = putStrLn (name ++ " is in Distinction category.")
  | g >= 50   = putStrLn (name ++ " is in Pass category.")
  | otherwise = putStrLn (name ++ " is in Fail category.")
checkGradeCategory (Student name (Score Nothing)) =
  putStrLn (name ++ " has no grade recorded.")

checkGradeCategoryIO :: Student -> IO ()
checkGradeCategoryIO s = do
  putStrLn "\n--- Grade Category ---"
  checkGradeCategory s
  putStrLn "Checked grade category successfully."
  pause

-- ==============================
-- Main Menu
-- ==============================

menu :: [Student] -> IO ()
menu students = do
  putStrLn "\n=== Mini Registrar System ==="
  putStrLn "1. Register a student"
  putStrLn "2. Show student info"
  putStrLn "3. Compute class total"
  putStrLn "4. Check grade category"
  putStrLn "5. Exit"
  putStr "Choose an option: "
  hFlush stdout
  choice <- getLine

  case choice of
    "1" -> do
      s <- registerStudent
      menu (students ++ [s])
    "2" -> case students of
      [] -> putStrLn "\nNo students registered yet." >> pause >> menu students
      _  -> mapM_ showStudent students >> menu students
    "3" -> computeClassTotal students >> menu students
    "4" -> case students of
      [] -> putStrLn "\nNo students registered yet." >> pause >> menu students
      _  -> mapM_ checkGradeCategoryIO students >> menu students
    "5" -> putStrLn "\nExiting Mini Registrar. Goodbye!"
    _   -> putStrLn "\nInvalid choice, try again." >> menu students

-- ==============================
-- Main
-- ==============================

main :: IO ()
main = menu []
