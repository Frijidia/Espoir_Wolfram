import System.Directory.Internal.Prelude (getArgs, exitFailure)

doRule::[Char] -> [Char]
doRule [] = []
doRule (a:b:_) = []
doRule (a:b:c:xs)
       | (a == '*' && b == '*' && c == '*') = " " ++ doRule xs
       | (a == '*' && b == '*' && c == ' ') = " " ++ doRule xs
       | (a == '*' && b == ' ' && c == '*') = " " ++ doRule xs
       | (a == ' ' && b == ' ' && c == ' ') = " " ++  doRule xs
       | otherwise = "*" ++ doRule xs

myrule::[Char] -> [Char] -> [Char]
myrule line nextline = doRule line

main::IO()
main = do
     args <- getArgs
     let line = myrule " * " ""
     print(line)
