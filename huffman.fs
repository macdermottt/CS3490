
// Huffman encoding homework assignment
//
// Author: Tobin MacDermott
// Date: Nov 14 2014
// Class: CS 3490 Fall 2014
//


module Huff
    
    type huffmanTreeType =
      | Leaf of char
      | Node of huffmanTreeType * huffmanTreeType

    let huffmanTable = 
        [ ('A',"1011");   ('B',"100000");   ('C',"01000");('D',"10101");    ('E',"110");
          ('F',"00000");  ('G',"100011");   ('H',"0110"); ('I',"1111");     ('J',"000011011");
          ('K',"0000111");('L',"10100");    ('M',"00011");('N',"1110");     ('O',"1001");
          ('P',"100001"); ('Q',"000011001");('R',"0101"); ('S',"0111");     ('T',"001");
          ('U',"01001");  ('V',"000010");   ('W',"00010");('X',"000011010");('Y',"100010");
          ('Z',"000011000")] 

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

    // explodes a string into a list of chars
    // params: the string to explode
    // returns: a list of the chars in the string
    let explode str = [ for chr in str -> chr ]

    // encodeChar - encodes one char according to the given 
    // huffmanTable above
    // params: the char to encode
    // returns: the string encoding for the char
    let encodeChar ch = 
        let rec findInTable table x = 
            match table with
            | [] -> [] 
            | (chr,value)::t -> if x = chr
                             then explode value
                             else findInTable t x 
        findInTable huffmanTable ch

    let encodeList lst = 
        [ for chr in lst do yield! ( encodeChar chr ) ]
        

    // implode - implodes a list of chars into a string
    // params: the list of chars to implode
    // returns: the string of the given chars
    let implode lst =
        let rec impl l accum =
            match l with
            | [] -> accum
            | h::t -> impl t (accum + new string( h, 1))
        impl lst ""
     

    let decodeList lst = 
        // decode_tr - recusive helper function to decode a list of zeros and ones
        // params: the list of zeros and ones
        //         accumulator - this should initially be an empty list
        // returns: a list of chars 
        let rec decode_tr lst accum = 
        
            // findCharInTree - recursive helper function to find one char in the given tree
            // params: the tree to search in 
            //         the list of zeros and ones to search with
            // returns: a tuple with the char found and the remaining list
            let rec findCharInTree tree lst = 
                match tree with 
                | Leaf( c ) -> ( c , lst )
                | Node( left, right ) -> match lst with 
                                         | [] -> printf "invalid encoding"
                                                 ( 'x', [] )
                                         | '0'::tail -> findCharInTree left tail
                                         | '1'::tail -> findCharInTree right tail


            match (findCharInTree huffmanTree lst) with
            | ( c , [] ) -> accum @ [c]
            | ( c , tail ) -> decode_tr tail (accum @ [c])

        decode_tr lst accum

    // decode - main decode function.
    //          decodes a string of zeros and ones into a string of chars
    // params: string of zeros and ones
    // returns: string of chars
    let decode = implode >> decodeList >> explode  
    
    // encode - main encode function
    //          encodes a string into a string of zeros and ones
    // params: the string to encode, must be solely comprized of capital chars
    // returns: a string of zeros and ones
    //let encode str = implode [for chr in (explode str) do yield!( explode( encodeChar chr) )]
    let encode = implode >> encodeList >> explode


    // a test function for good measure
    let test = 
        lazy(
        
        let bFox =  "THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOG" 
        let encodedFox = encode bFox
        let decodedFox = decode encodedFox

        printfn "original: %s" bFox
        printfn "encoded: %s"  encodedFox
        printfn "decoded: %s"  decodedFox
        if bFox = decodedFox
            then printfn "TEST PASSED"
            else printfn "TEST FAILED"
        )
         
        
