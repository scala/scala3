object Test {
  if (true) {
    <br>                            // error maybe this tag isn't closed // error
  }else{                            // error // error in XML content, use double brace
    <span>{"louenesee"}</span>
  }
}   // anypos-error
