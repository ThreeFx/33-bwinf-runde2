UNIT SuffixTree;

INTERFACE

  TYPE
    Nptr = ^TNode;
  
    TNode = RECORD
      child        : Nptr;
      next_sibling : Nptr;
      offset       : Integer;
      len          : Integer;
    END;
  
    TSuffixTree = RECORD
      root : TNode;
    END;

  FUNCTION CreateSuffixTreeFrom(s : String) : TSuffixTree;

IMPLEMENTATION

  FUNCTION GetNodeAt(node : Nptr; s : String; c : Char) : Nptr;
  VAR
    result : Nptr;
  BEGIN
    result := nil;
    WHILE node <> nil DO
    BEGIN
      IF s[node^.offset + 1] = c THEN
      BEGIN
        result := node;
        exit;
      END;
      node := node^.next_sibling;
    END;
  END;

  FUNCTION CreateSuffixTreeFrom(s : String) : TSuffixTree;
  VAR
    i, j, k, len, offset : Integer;
    cur, child, mid, newNode : Nptr;
    root : TNode;
    result : TSuffixTree;
  BEGIN
    s := s + '$';

    root.offset := 0;
    root.len := Length(s);
    root.next_sibling := nil;
    root.child := nil;

    FOR i := 2 TO Length(s) DO
    BEGIN
      cur := Addr(root);
      j := i;

      WHILE j < Length(s) DO
      BEGIN
        child := GetNodeAt(cur^.child, s, s[j]);

        IF child <> nil THEN
        BEGIN
          offset := child^.offset;
          len := child^.len;

          k := j + 1;
          WHILE (k - j < len) AND (s[k] = s[offset + k - j + 1]) DO
          BEGIN
            Inc(k);
          END;

          IF k - j = len THEN
          BEGIN
            cur := child;
            j := k;
          END
          ELSE
          BEGIN
            newNode := GetMem(SizeOf(TNode));
            newNode^.offset := k;
            newNode^.len := Length(s) - k;
            newNode^.next_sibling := cur^.child;
            newNode^.child := nil;

            mid := GetMem(SizeOf(TNode));
            mid^.offset := j;
            mid^.len := k - j;
            mid^.next_sibling := nil;
            mid^.child := newNode;
            
            cur^.child := mid;
          END;
        END
        ELSE
        BEGIN
          child := GetMem(SizeOf(TNode));
          child^.offset := j - 1;
          child^.len := Length(s) - cur^.offset;
          child^.next_sibling := cur^.child;
          child^.child := nil;

          cur^.child := child;
        END;
      END;
    END;
    result.root := root;
  END;
 
BEGIN
END.
