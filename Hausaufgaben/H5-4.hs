-- Vorlage fuer H5-4

data HuffmanTree = Leaf Char
                 | Node HuffmanTree HuffmanTree
                   deriving Show

german :: HuffmanTree
german = Node (Node (Node (Leaf 'n') (Node (Leaf 'd') (Node (Node (Leaf 'k') (Leaf 'f')) (Leaf 'o')))) (Node (Node (Node (Leaf 'c') (Leaf 'g')) (Leaf 't')) (Node (Leaf 's') (Node (Node (Node (Leaf 'v') (Node (Node (Node (Node (Leaf 'q') (Leaf 'y')) (Leaf 'x')) (Leaf 'j')) (Leaf 'p'))) (Leaf 'b')) (Node (Node (Leaf 'z') (Leaf 'w')) (Leaf 'm')))))) (Node (Node (Leaf ' ') (Node (Leaf 'r') (Node (Leaf 'u') (Leaf 'l')))) (Node (Leaf 'e') (Node (Leaf 'i') (Node (Leaf 'h') (Leaf 'a')))))

decode :: HuffmanTree -> String -> String
decode t x = error "TODO"
