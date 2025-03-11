module Gitignore (
  PlainPattern(..),
  checkGitignoreExists,
  createGitignore,
  checkLastCharIsNewLine,
  readGitignoreContent,
  appendToGitignore,
  removeGitignore,
  renameFileToGitignore,
  writeToTempGitignore,
  module Gitignore.Internal
)

where


import Gitignore.Internal

-- domain types --
newtype PlainPattern = PlainPattern String


-- public API --

checkGitignoreExists :: (MonadCheckFileExistence m) => m Bool
checkGitignoreExists = checkFileExists ".gitignore"

createGitignore :: (MonadAppendFile m) => m ()
createGitignore = do
  appendToFile ".gitignore" "## .gitignore created by Gitignore"

checkLastCharIsNewLine :: (MonadCheckLastCharIsNewLine m) => m Bool
checkLastCharIsNewLine =
  checkIfNewLineEnding ".gitignore"

readGitignoreContent :: (MonadReadContent m) => m String
readGitignoreContent = readContent ".gitignore"

appendToGitignore :: (MonadAppendFile m) => String -> m ()
appendToGitignore = appendToFile ".gitignore"

removeGitignore :: (MonadCanDeleteFile m) => m ()
removeGitignore = deleteFile ".gitignore"

renameFileToGitignore :: (MonadCanRenameFile m) => FilePath -> m ()
renameFileToGitignore = flip changeFileName ".gitignore"

writeToTempGitignore :: (MonadCanWriteToTempFile m) => String -> m FilePath
writeToTempGitignore = writeToTempFile "." ".gitignore~"