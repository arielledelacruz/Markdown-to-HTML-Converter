module Assignment (nestedParser, markdownParser, convertADTHTML, plainTextParser, italicParser, boldParser, strikethroughParser, linkParser, inlineCodeParser, footnoteParser, headingParser, imageParser, blockQuoteParser, codeBlockParser, orderedListParser) where


import           Control.Applicative   (Alternative((<|>)), many, some, optional)
import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..), parse)
import           Parser           (is, string, satisfy, digit, is, space, spaces)

data ADT
    = PlainText String
    | ItalicText String
    | BoldText String
    | StrikethroughText String
    | LinkText String String
    | InlineCode String
    | Footnote Int
    | FootnoteReference Int String
    | Heading Int [ADT]   
    | BlockQuote [ADT]
    | CodeBlock String String
    | OrderedList [ADT]
    | OrderedListItem String
    | Table [String] [[String]]
    | Image String String String  -- AltText, URL, Caption
    | Document [ADT]
    deriving (Show, Eq)

-- Parser for nested elements
nestedParser :: Parser ADT
nestedParser = imageParser
             <|> boldParser
             <|> italicParser
             <|> strikethroughParser
             <|> linkParser
             <|> inlineCodeParser
             <|> footnoteReferenceParser
             <|> footnoteParser
             <|> blockQuoteParser
             <|> codeBlockParser
             <|> headingParser
             <|> plainTextParser

-- Parser for the entire document
markdownParser :: Parser ADT
markdownParser = Document <$> many nestedParser

-- Parser for plain text, used as a fallback when no other markdown is found
plainTextParser :: Parser ADT
plainTextParser = do
    content <- some (satisfy (`notElem` specialChars))  -- `some` ensures we match at least one character
    pure (PlainText content)

-- Define special characters that indicate markdown syntax
specialChars :: [Char]
specialChars = ['*', '_', '~', '[', '`', '#', '>', '-', '|', '!']


-- Italic parser
italicParser :: Parser ADT
italicParser = do
    _ <- is '_'                 -- Match the starting underscore
    content <- many (satisfy (/= '_'))  -- Parse until the next underscore
    _ <- is '_'                 -- Match the closing underscore
    pure (ItalicText content)

-- Bold parser
boldParser :: Parser ADT
boldParser = do
    _ <- string "**"              -- Match the starting double asterisks
    content <- many (satisfy (/= '*'))  -- Parse until the next double asterisk
    _ <- string "**"              -- Match the closing double asterisks
    pure (BoldText content)

-- Strikethrough parser
strikethroughParser :: Parser ADT
strikethroughParser = do
    _ <- string "~~"              -- Match the starting double tilde
    content <- many (satisfy (/= '~'))  -- Parse until the next double tilde
    _ <- string "~~"              -- Match the closing double tilde
    pure (StrikethroughText content)

-- Link parser
linkParser :: Parser ADT
linkParser = do
    _ <- is '['                 -- Match the opening square bracket
    text <- many (satisfy (/= ']'))  -- Parse the link text
    _ <- string "]("
    url <- many (satisfy (/= ')'))  -- Parse the URL
    _ <- is ')'                 -- Match the closing parenthesis
    pure (LinkText text url)

-- Inline code parser
inlineCodeParser :: Parser ADT
inlineCodeParser = do
    _ <- is '`'                 -- Match the backtick
    content <- many (satisfy (/= '`'))  -- Parse the content inside backticks
    _ <- is '`'                 -- Match the closing backtick
    pure (InlineCode content)

-- Footnote parser
footnoteParser :: Parser ADT
footnoteParser = do
    _ <- string "[^"              -- Match the start of the footnote
    number <- some digit          -- Parse the footnote number
    _ <- is ']'                   -- Match the closing bracket
    pure (Footnote (read number))  -- Convert the number string to an integer

-- Image parser
imageParser :: Parser ADT
imageParser = do
    _ <- string "!["                -- Match the ![
    altText <- many (satisfy (/= ']')) -- parse alt text until closing bracket
    _ <- string "]("
    url <- many (satisfy (/= ' ')) -- parse URL until space (no space allowed in URL)
    _ <- space                      -- require exactly one space before the caption
    _ <- is '"'                     -- match the starting double quote
    caption <- many (satisfy (/= '"')) -- parse caption until the closing double quote
    _ <- is '"'                     -- match the closing double quote
    _ <- is ')'                     -- match the closing parenthesis
    pure (Image altText url caption) -- return the parsed image



-- Footnote reference parser
footnoteReferenceParser :: Parser ADT
footnoteReferenceParser = do
    _ <- string "[^"
    number <- some digit -- Parse the number
    _ <- is ']'
    _ <- is ':' -- parse colon after footnote reference
    content <- many (satisfy (/= '\n')) -- parse until end of line
    pure (FootnoteReference (read number) content)

-- Heading parser (Hash syntax and Alternative syntax)
headingParser :: Parser ADT
headingParser = hashHeadingParser <|> altHeadingParser

-- Parses headings with hash syntax
hashHeadingParser :: Parser ADT
hashHeadingParser = do
    hashes <- some (is '#') -- match one or more '#'
    _ <- space
    content <- many nestedParser -- parse the rest of the line as nested markdown
    let level = length hashes -- determine heading level based on number of '#'
    pure (Heading level content)

-- Parses the alternative syntax for level 1 and level 2
altHeadingParser :: Parser ADT
altHeadingParser = do
    headingText <- many (satisfy (/= '\n')) -- Read the heading text
    _ <- is '\n'
    headingLevel <- altHeadingLevelParser
    pure (Heading headingLevel [PlainText headingText])

-- Detects if the alternative syntax is level 1 (===) or level 2 (---)
altHeadingLevelParser :: Parser Int
altHeadingLevelParser = do
    _ <- spaces -- Skip any leading spaces before the `=` or `-`
    levelMarker <- many (is '=' <|> is '-')
    _ <- is '\n'
    if all (== '=') levelMarker
        then pure 1
        else pure 2

-- Blockquote parser
blockQuoteParser :: Parser ADT
blockQuoteParser = do
    _ <- optional space
    _ <- is '>'
    _ <- optional space -- Ignore leading space after '>'
    content <- many (satisfy (/= '\n')) -- Parse until the end of the line
    _ <- is '\n'
    remaining <- many (blockQuoteLine <|> pure "") -- Continue parsing subsequent lines
    pure (BlockQuote (PlainText content : map PlainText remaining))

-- Helper to parse subsequent lines starting with '>'
blockQuoteLine :: Parser String
blockQuoteLine = do
    _ <- optional space
    _ <- is '>'
    _ <- optional space
    many (satisfy (/= '\n')) -- Parse the rest of the line


-- Code block parser
codeBlockParser :: Parser ADT
codeBlockParser = do
    _ <- optional (many (is ' '))  -- Allow leading spaces
    _ <- string "```"              -- Match the opening backticks
    lang <- many (satisfy (/= '\n')) -- Parse optional language identifier
    _ <- is '\n'
    code <- many (satisfy (/= '`')) -- Parse until the closing backticks
    _ <- string "```"              -- Match the closing backticks
    pure (CodeBlock lang code)

--- Ordered list parser
orderedListParser :: Parser ADT
orderedListParser = do
    items <- some orderedListItemParser
    pure (OrderedList items)

-- Parser for individual list items
orderedListItemParser :: Parser ADT
orderedListItemParser = do
    _ <- some digit -- Start with a number
    _ <- is '.'
    _ <- space
    content <- many (satisfy (/= '\n')) -- Parse list item content
    subItems <- many (subListParser <|> pure "")
    pure (OrderedListItem (content ++ concat subItems))

-- Parser for sub-items indented by 4 spaces
subListParser :: Parser String
subListParser = do
    _ <- string "    " -- Exactly 4 spaces
    _ <- some digit -- Sublist starts with a number
    _ <- is '.'
    _ <- space
    content <- many (satisfy (/= '\n'))
    pure ("\n    " ++ content)

-- -- Table parser
-- tableParser :: Parser ADT
-- tableParser = do
--     header <- tableRowParser
--     _ <- tableSeparator
--     rows <- many tableRowParser
--     pure (Table header rows)

-- -- Parses a single table row
-- tableRowParser :: Parser [String]
-- tableRowParser = do
--     _ <- is '|'
--     cells <- sepBy tableCellParser (is '|')
--     _ <- is '|'
--     _ <- is '\n'
--     pure cells

-- -- Parses an individual table cell
-- tableCellParser :: Parser String
-- tableCellParser = do
--     spaces -- Ignore leading spaces
--     content <- many (satisfy (/= '|')) -- Parse cell content
--     spaces -- Ignore trailing spaces
--     pure content

-- -- Parses the separator row (e.g. `| --- | --- |`)
-- tableSeparator :: Parser ()
-- tableSeparator = do
--     _ <- is '|'
--     _ <- many (is '-' <|> is ' ')
--     _ <- is '|'
--     _ <- is '\n'
--     pure ()

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime


convertADTHTMLList :: [ADT] -> String
convertADTHTMLList = concatMap convertADTHTML

convertADTHTML :: ADT -> String
convertADTHTML (PlainText text)       = text
convertADTHTML (ItalicText text)      = "<em>" ++ text ++ "</em>"
convertADTHTML (BoldText text)        = "<strong>" ++ text ++ "</strong>"
convertADTHTML (StrikethroughText text) = "<del>" ++ text ++ "</del>"
convertADTHTML (LinkText text url)    = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode code)      = "<code>" ++ code ++ "</code>"
convertADTHTML (Footnote number)      = "<sup>[" ++ show number ++ "]</sup>"
convertADTHTML (Image alt url caption) = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\"/>"
convertADTHTML (Heading level content) = "<h" ++ show level ++ ">" ++ convertADTHTMLList content ++ "</h" ++ show level ++ ">"
convertADTHTML (BlockQuote content)    = "<blockquote>" ++ convertADTHTMLList content ++ "</blockquote>"
convertADTHTML (CodeBlock lang code)   = "<pre><code class=\"" ++ lang ++ "\">" ++ code ++ "</code></pre>"
convertADTHTML (OrderedList items)     = "<ol>" ++ concatMap (\item -> "<li>" ++ convertADTHTML item ++ "</li>") items ++ "</ol>"
convertADTHTML (OrderedListItem text)  = "<li>" ++ text ++ "</li>"
convertADTHTML (Table header rows)     = "<table><thead><tr>" ++ concatMap (\h -> "<th>" ++ h ++ "</th>") header ++ "</tr></thead><tbody>" ++ concatMap (\row -> "<tr>" ++ concatMap (\cell -> "<td>" ++ cell ++ "</td>") row ++ "</tr>") rows ++ "</tbody></table>"
convertADTHTML (FootnoteReference number content) = "<sup>[" ++ show number ++ "]</sup>: " ++ content
convertADTHTML (Document content) = "<div>" ++ convertADTHTMLList content ++ "</div>"