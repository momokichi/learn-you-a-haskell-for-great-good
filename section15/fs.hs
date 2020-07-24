import           Data.List (break)

x -: f = f x

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs) , bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "hoge.hs" "hogee",
      File "fuga.hs" "fugaa",
      Folder "baa" [ File "foo.js" "fooo", File "bar.js" "barr"]
    ]

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs)     = (File newName dat, bs)

fsNewItem :: FSItem -> FSZipper -> FSZipper
fsNewItem item (Folder folderName items, bs) =
  (Folder folderName (item:items), bs)
