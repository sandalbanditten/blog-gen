-- Command-line options parsing

module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import           Data.Maybe          (fromMaybe)
import           Options.Applicative

-- * The options

-- The model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

-- A single input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

-- A single output sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show


-- * The parser

-- Parse CLI options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info (pOptions <**> helper)
    ( fullDesc
      <> header "blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to HTML"
    )

-- Parse all options
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to HTML")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to HTML")
      )
    )

-- * Single source to single sink parser

-- Single source to single sink options
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

-- Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

-- Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- Input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

-- Input file parser
pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

-- * Directory conversion parser

-- Parser for directory
pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir

-- Parser for input directory
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

-- Parser for output directory
pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )
