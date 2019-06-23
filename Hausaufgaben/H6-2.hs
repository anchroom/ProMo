
data HuffmanTree = Leaf Char
                 | Node HuffmanTree HuffmanTree
                   deriving Show

german :: HuffmanTree
german = Node (Node (Node (Leaf 'n') (Node (Leaf 'd') (Node (Node (Leaf 'k') (Leaf 'f')) (Leaf 'o')))) (Node (Node (Node (Leaf 'c') (Leaf 'g')) (Leaf 't')) (Node (Leaf 's') (Node (Node (Node (Leaf 'v') (Node (Node (Node (Node (Leaf 'q') (Leaf 'y')) (Leaf 'x')) (Leaf 'j')) (Leaf 'p'))) (Leaf 'b')) (Node (Node (Leaf 'z') (Leaf 'w')) (Leaf 'm')))))) (Node (Node (Leaf ' ') (Node (Leaf 'r') (Node (Leaf 'u') (Leaf 'l')))) (Node (Leaf 'e') (Node (Leaf 'i') (Node (Leaf 'h') (Leaf 'a')))))

-- |
{--
codes :: HuffmanTree -> [(Char, String)]
codes (Leaf c)          = [(c, "")]
codes (Node l (Leaf c)) = (codes l) ++ [(c, "1")]
codes (Node (Leaf c) r) = [(c,"0")] ++ (codes r)
codes (Node l r)        = (map ('0':) snd (codes l)) ++ (codes r)
--codes (Node l r) = codeHelper t (Node l r) ""
-}

-- first tree for the whole stucture
-- second tree is the latest position
-- first String is the current tree path, or latest output String
codeHelper ::  HuffmanTree  -> String
codeHelper ( Leaf c )  = ""
--codeHelper ( Leaf c )  = xs
codeHelper ( Node l r )  = (('0':)  <$> codeHelper l) ++ (('1':) <$> codeHelper r)

--codeHelper t r ('1':xs ) = codeHelper t ( Node l r ) xs
--codeHelper t ( Node l r ) " " = " "
-- alles was keine 0 oder 1 ist bleibt unveraendert
--codeHelper t ( Node l r ) ( s : ss ) = s : ( decodeHelper t t ss )

{--

-- to encode tree
-- first HuffmanTree is for



encode :: HuffmanTree -> String -> String
encode tree text =  error "TODO"
-}