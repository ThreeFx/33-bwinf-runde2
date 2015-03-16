{ (c) Ben Fiedler 2015 }

PROGRAM Mississippi;

USES SuffixTree;

VAR
 	tree : TSuffixTree;

BEGIN

  tree := CreateSuffixTreeFrom('abaaba');
  Write('Press any key to exit...');
  Read();

END.
