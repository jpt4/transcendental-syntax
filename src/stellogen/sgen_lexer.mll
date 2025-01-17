{
  open Sgen_parser
  exception SyntaxError of string
}

let alpha    = ['a'-'z' 'A'-'Z']
let num      = ['0'-'9']
let alphanum = alpha | num
let ident    = ['a'-'z' '0'-'9'] (alphanum | [ '_' '?'])* '\''*
let var_id   = ['A'-'Z'] (alphanum | ['_' '-'])* '\''*
let space    = [' ' '\t']+
let newline  = '\r' | '\n' | "\r\n"

rule read = parse
  (* Stellogen *)
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | "end"     { END }
  | "show"    { SHOW }
  | "galaxy"  { GALAXY }
  | "print"   { PRINT }
  | "process" { PROCESS }
  | "->"      { RARROW }
  | "=>"      { DRARROW }
  | "."       { DOT }
  | '"'       { read_string (Buffer.create 255) lexbuf }
  (* Stellar resolution *)
  | '_'       { PLACEHOLDER }
  | '['       { LBRACK }
  | ']'       { RBRACK }
  | '('       { LPAR }
  | ')'       { RPAR }
  | ','       { COMMA }
  | '@'       { AT }
  | '$'       { DOLLAR }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '='       { EQ }
  | ':'       { CONS }
  | ';'       { SEMICOLON }
  | var_id    { VAR (Lexing.lexeme lexbuf) }
  | ident     { SYM (Lexing.lexeme lexbuf) }
  (* Common *)
  | '\''      { comment lexbuf }
  | "'''"     { comments lexbuf }
  | space     { read lexbuf }
  | newline   { EOL }
  | eof       { EOF }
  | _         {
    raise (SyntaxError
      ("Unexpected character '" ^
      (Lexing.lexeme lexbuf) ^
      "' during lexing"))
  }

and read_string buf = parse
  | '"'       { SYM ("\""^(Buffer.contents buf)^"\"") }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ {
    raise (SyntaxError
      ("Illegal string character: " ^ Lexing.lexeme lexbuf))
    }
  | eof { raise (SyntaxError ("String is not terminated")) }

and comment = parse
  | newline        { EOL }
  | eof            { EOF }
  | _              { comment lexbuf }

and comments = parse
  | "'''"    { read lexbuf }
  | _        { comments lexbuf }
