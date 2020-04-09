module Main where

import Data.Char (isAlpha)
import Text.Read (readMaybe)
import Cipher (cipher, vigenereCipher)

main :: IO ()
main = do
    cipherType <- askCipherType
    case cipherType of
        "c" -> handleCipher
        "v" -> handleVCipher
        _   -> main

askCipherType :: IO String
askCipherType = do
    putStrLn "Select (c)ipher or (v)igenere cipher"
    getLine

handleCipher :: IO ()
handleCipher = do
    putStrLn "Specify shift value (interger)"
    shiftValTxt <- getLine
    case (readMaybe shiftValTxt :: Maybe Integer) of
        Just shiftVal -> do
            word <- askWord
            putStrLn $ cipher (fromIntegral shiftVal) word
        Nothing -> handleCipher

handleVCipher :: IO ()
handleVCipher = do
    putStrLn "Specify keyword (alphabet only)"
    keyword <- getLine
    if all isAlpha keyword
        then do
            word <- askWord
            putStrLn $ vigenereCipher keyword word
        else handleVCipher

askWord :: IO String
askWord = do
    putStrLn "Provide word to cipher"
    getLine
