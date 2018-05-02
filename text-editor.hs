import Text.Show.Functions
import System.IO
import Text.Printf
-----------------Text Editor--------------------
--Definition for the text editor's parameters (b,k,p,h,z)
--b = left right of text, k = cursor, p = right side of text, h = highlight set, z = clipboard set
data TextEditor a = Sets([Char],Char,[Char],[Char],[Char])

--instance of a text editor with default params
text = Sets("",'|',"",[],[])

--when TextEditor instance are shown i.e. printed, they are printed in text=[] highlights=[] clipboard=[] --format}
instance (Show a) => Show (TextEditor a) where
    show (Sets(b,k,p,h,z)) = printf("text=["++b++k:[]++p++"]"++" highlights=["++h++"]"++" clipboard=["++z++"]")
    
--takes a list of strings from user input and concatinates with the left hand side of the cursor
userInput:: (TextEditor e, [Char]) -> TextEditor(e)
userInput(Sets(b,k,p,h,z),inString)= Sets(b++inString,k,p,h,z) 

--moves cursor left by moving characters from the left of the cursor (b) to the right (p)
--move can only performed if left list (b) is not empty when moving, else recursivly call itself with --empty highlight set (append the highlight to characters to the right(p))
moveCursorLeft:: TextEditor(e) -> TextEditor(e)
moveCursorLeft(Sets(b,k,p,h,z))
    |b == []                    = Sets(b,k,p,h,z)
    |h == []                    = Sets(reverse(tail(reverse(b))),k,[head(reverse(b))]++p,h,z) 
    |otherwise                  = moveCursorLeft(Sets(b,k,h++p,[],z))
    
--moves cursor right by moving characters from the right of the cursor(p) to the left(b)
--move can only performed if right list (p) is not empty when moving, else recursivly call itself with --empty highlight set (append the highlight to characters to the left(p))
moveCursorRight:: TextEditor(e) -> TextEditor(e)
moveCursorRight(Sets(b,k,p,h,z))
    |p == []                    = Sets(b,k,p,h,z)
    |h == []                    = Sets(b++[head(p)],k,tail(p),h,z)
    |otherwise                  = moveCursorRight(Sets(b++h,k,p,[],z))

--Sets the cursor to the left word by recursivly calling SetCursorWordLeft(moveCursorLeft()) until empty --space is encountered or left(b) set becomes empty
setCursorWordLeft::TextEditor(e) -> TextEditor(e)
setCursorWordLeft(Sets(b,k,p,h,z))
    |b == []                    = Sets(b,k,p,h,z)
    |head(reverse(b)) == ' '    = moveCursorLeft(Sets(b,k,p,h,z))
    |otherwise                  = setCursorWordLeft(moveCursorLeft(Sets(b,k,p,h,z)))

--Sets the cursor to the right word by recursivly calling SetCursorWordRight(moveCursorRight()) until --empty space is encountered or right(p) set becomes empty
setCursorWordRight::TextEditor(e) -> TextEditor(e)
setCursorWordRight(Sets(b,k,p,h,z))
    |p == []                    = Sets(b,k,p,h,z)
    |head(p) == ' '             = moveCursorRight(Sets(b,k,p,h,z))
    |otherwise                  = setCursorWordRight(moveCursorRight(Sets(b,k,p,h,z)))

--adds characters from the left side of the cursor(b) to the highlight set and removes it from the left side
--reverse("hello") -> "olleh" -> tail("olleh") -> "lleh" -> reverse("hell") removes character from left
--reverse("hello") -> "olleh" -> head("olleh") -> 'o' 'o'++[] = [o] appends removed char to highlights
highlightCharLeft::TextEditor(e) -> TextEditor(e)
highlightCharLeft(Sets(b,k,p,h,z))= Sets(reverse(tail(reverse(b))),k,p,[head(reverse(b))]++h,z)

--adds characters from the right side of the cursor(p) to the highlight set and removes it from the right side
--tail("hello") -> "ello" removes character from the right side of the set (closest to cursor)
--concatinates the remmoved letter i.e. 'h' to the left side of highlights
highlightCharRight::TextEditor(e) -> TextEditor(e)
highlightCharRight(Sets(b,k,p,h,z))= Sets(b,k,tail(p),h++[head(p)],z)

--highlight a word to the left by recursivly calling higher order function containing highlight char left
--when empty space is detected, it highlights it (to stop program from getting stuck) and stops
highlightWordLeft(Sets(b,k,p,h,z))
    |b == []                    = Sets(b,k,p,h,z)
    |head(reverse(b)) == ' '    = highlightCharLeft(Sets(b,k,p,h,z))
    |otherwise                  = highlightWordLeft(highlightCharLeft(Sets(b,k,p,h,z)))

--higlights a word to the right by recursivly calling higher-order function containing highlightCharRight
--when empty space is detected, it highlights it (to stop program from getting stuck) and returns the highlighted text editor   
highlightWordRight(Sets(b,k,p,h,z))
    |p == []                    = Sets(b,k,p,h,z)
    |head(p) == ' '             = highlightCharRight(Sets(b,k,p,h,z))
    |otherwise                  = highlightWordRight(highlightCharRight(Sets(b,k,p,h,z)))

--sets the cursor to the home by simply removing all the characters to the left of the cursor(b) and
--concatinating the b set to the left side of highlight set (avoids order confusion) and passes empty b --set back to the text editor with concatinated highlight set
highlightCursorToHead::TextEditor(e) -> TextEditor(e)
highlightCursorToHead(Sets(b,k,p,h,z)) = Sets([],k,p,b++h,z)

--sets the cursor to the end (tail) by removing all the characters from the right set of the cursor (p) and
--concatinating that removed set to the right side of highlight set
highlightCursorToTail::TextEditor(e) -> TextEditor(e)
highlightCursorToTail(Sets(b,k,p,h,z)) = Sets(b,k,[],h++p,z)

--highlights both sides of the cursor by returning both sides as empty lists and concatinating left side (b)
--to the left of higlights (h) and concatinating rightside(p) to the right of highlights
highlightAll::TextEditor(e) -> TextEditor(e)
highlightAll(Sets(b,k,p,h,z)) = Sets([],k,[],h++b++p,z)

--sets the cursor to the start of the text by returning an empty list to the left side of the text editor
--and concatinating the removed text from the left (b) and right (p) to highlights (h)
--when this is done, whatever was highlighted before, should be unhighlighted (passing empty h set if /= [])
setCursorStart::TextEditor(e) -> TextEditor(e)
setCursorStart(Sets(b,k,p,h,z)) 
    |h /= []                    = Sets([],k,b++p++h,[],z)
    |otherwise                  = Sets([],k,b++p++h,h,z)

--sets the cursor to the end of the text by returning empty left set (p) and concatinating highlight set (h)
--to the left of left set(b) and concatinating the right set(p) to the right of right set(b)
--if when doing this operation the highlight is not empty, it should be emptied to say that it's deselected
setCursorEnd::TextEditor(e) -> TextEditor(e)
setCursorEnd(Sets(b,k,p,h,z))
    |h /= []                    = Sets(h++b++p,k,[],[],z)
    |otherwise                  = Sets(h++b++p,k,[],h,z)

--works by sending whatever is highlighted(h) to the clipboard(z)
copy::TextEditor(e) -> TextEditor(e)
copy(Sets(b,k,p,h,z)) = Sets(b,k,p,h,h)

--pastes all characters from the clipboard(z) to the right of left character set(b) and clears what was highlighted(h)
paste::TextEditor(e) -> TextEditor(e)
paste(Sets(b,k,p,h,z)) = Sets(b++z,k,p,[],z)

--cut just empties the highlighted set and sets the clipboard(z) to the highlighted(z) set
cut::TextEditor(e) -> TextEditor(e)
cut(Sets(b,k,p,h,z)) = Sets(b,k,p,[],h)

--removes character to the left of the cursor(b)
--reverse("Hello") -> "olleH" -> tail("olleH") -> "lleH" -> reverse("lleH") -> "Hell"
backspace::TextEditor(e) -> TextEditor(e)
backspace(Sets(b,k,p,h,z)) = Sets(reverse(tail(reverse(b))),k,p,h,z)

--empties highlighted set (unlike cut, doesn't pass the removed set to clipboard)
deleteSelection::TextEditor(e) -> TextEditor(e)
deleteSelection(Sets(b,k,p,h,z)) = Sets(b,k,p,[],z)

--loads contents of given fileName and returns a text editor
load::([Char],TextEditor(e)) -> IO(TextEditor(e))
load(fileName,Sets(b,k,p,h,z)) = do
    contents <-readFile fileName
    putStr("You have successfully loaded: "++fileName++"\n")
    return(Sets(contents,k,[],[],[]))

--saves the text editor to the desired fileName
save::([Char],TextEditor(e)) -> IO(TextEditor(e))
save(fileName, Sets(b,k,p,h,z)) = do
    writeFile fileName b
    putStr("You have successfully saved to: "++fileName++"\n")
    return(Sets(b,k,p,h,z))