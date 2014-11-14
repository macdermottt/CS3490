
// Huffman encoding homework assignment
//
// Author: Tobin MacDermott
// Date: Nov 14 2014
// Class: CS 3490 Fall 2014
//


module Huff

    let rec zip L1 L2 =
      match L1 with
      | [] -> []
      | h1::t1 -> match L2 with
                  | [] -> []
                  | h2::t2 -> (h1,h2) :: (zip t1 t2)
    let asciiTable = zip [for c in 'A'..'Z' do yield c] [for i in 65..90 do yield i]


    let huffmanTable = 
        [ ('A',"1011");   ('B',"100000");   ('C',"01000");('D',"10101");    ('E',"110");
          ('F',"00000");  ('G',"100011");   ('H',"0110"); ('I',"1111");     ('J',"000011011");
          ('K',"0000111");('L',"10100");    ('M',"00011");('N',"1110");     ('O',"1001");
          ('P',"100001"); ('Q',"000011001");('R',"0101"); ('S',"0111");     ('T',"001");
          ('U',"01001");  ('V',"000010");   ('W',"00010");('X',"000011010");('Y',"100010");
          ('Z',"000011000")] 


    type huffmanTreeType =
      | Leaf of char
      | Node of huffmanTreeType * huffmanTreeType

    let huffmanTree =
      Node(Node(Node(Node(Node(Leaf('F'),
                               Node(Leaf('V'),
                                    Node(Node(Node(Leaf('Z'),
                                                   Leaf('Q')),
                                              Node(Leaf('X'),
                                                   Leaf('J'))),
                                         Leaf('K')))),
                          Node(Leaf('W'),
                               Leaf('M'))),
                     Leaf('T')),
                Node(Node(Node(Leaf('C'),
                               Leaf('U')),
                          Leaf('R')),
                     Node(Leaf('H'),
                          Leaf('S')))),
           Node(Node(Node(Node(Node(Leaf('B'),
                                    Leaf('P')),
                               Node(Leaf('Y'),
                                    Leaf('G'))),
                          Leaf('O')),
                     Node(Node(Leaf('L'),
                               Leaf('D')),
                          Leaf('A'))),
                Node(Leaf('E'),
                     Node(Leaf('N'),
                          Leaf('I')))))

    let explode str = [ for chr in str -> chr ]

    let encodeChar ch = 
        let rec findInTable table x = 
            match table with
            | [] -> "" 
            | (chr,vl)::t -> if x = chr
                             then vl
                             else findInTable t x 
        findInTable huffmanTable ch
    

    // decode part

    let implode lst =
        let rec impl l accum =
            match l with
            | [] -> accum
            | h::t -> impl t (accum + new string( h, 1))
        impl lst ""
     


    let rec findCharInTree tree lst1 = 
        match tree with 
        | Leaf( c ) -> ( c , lst1 )
        | Node( left, right ) -> match lst1 with 
                                 | [] -> printf "invalid"
                                         ( 'x', [] )
                                 | '0'::tail -> findCharInTree left tail
                                 | '1'::tail -> findCharInTree right tail

    let rec decodeList lst accum = 
        match (findCharInTree huffmanTree lst) with
        | ( c , [] ) -> accum @ [c]
        | ( c , h::t ) -> decodeList (h::t) (accum @ [c])


    let decode str = implode ( decodeList ( explode str ) [] ) 
    
    let encode str = implode [for chr in (explode str) do yield!( explode( encodeChar chr) )]
    
        
