UNIT SuffixTree;

INTERFACE

	TYPE
		Nptr = ^TNode;

		TNode = RECORD
			offset       : Integer;
			len          : Integer;
			next_sibling : Nptr;
			child        : Nptr;
		END;

		TSuffixTree = RECORD
			root : TNode;
			s    : String;
			nodes : Integer;
		END;

	FUNCTION CreateSuffixTreeFrom(s : String) : TSuffixTree;
	FUNCTION GetString(node : TNode; s : String) : String;

IMPLEMENTATION

	FUNCTION GetNodeAt(node : Nptr; s : String; c : Char) : Nptr;
	{VAR
		result : Nptr;}
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
		i, j, k, len, offset, nodes : Integer;
		cur, child, mid, newNode : Nptr;
		root : TNode;
		{result : TSuffixTree;}
	BEGIN
		s := s + '$';

		cur := GetMem(SizeOf(TNode));
		cur^.offset := 0;
		cur^.len := Length(s);
		cur^.next_sibling := nil;
		cur^.child := nil;

		root.offset := 0;
		root.len := 0;
		root.next_sibling := nil;
		root.child := cur;

		nodes := 2;

		WriteLn('Root node: ',GetString(cur^, s));

		FOR i := 2 TO Length(s) DO
		BEGIN
			WriteLn('Iteration #',i - 1);
			cur := Addr(root);
			j := i;

			WHILE j <= Length(s) DO
			BEGIN
				child := GetNodeAt(cur^.child, s, s[j]);

				IF child <> nil THEN
				BEGIN
					offset := child^.offset;
					len := child^.len;

					k := j + 1;
					WHILE (k - j < len) AND (s[k] = s[offset + 1 + k - j]) DO
					BEGIN
						Inc(k);
					END;

					IF (k - j) = len THEN
					BEGIN
						WRiteLn('Full traverse of: ', GetString(child^, s),'; continuing');
						cur := child;
						j := k;
					END
					ELSE
					BEGIN
						newNode := GetMem(SizeOf(TNode));
						newNode^.offset := j + child^.offset;
						newNode^.len := Length(s) - newNode^.offset;
						newNode^.next_sibling := child;
						newNode^.child := nil;

						mid := GetMem(SizeOf(TNode));
						mid^.offset := child^.offset;
						mid^.len := k - j;
						mid^.next_sibling := cur^.child;
						mid^.child := newNode;

						child^.offset := child^.offset + (k - j);
						child^.len := child^.len - (k - j);

						cur^.child := mid;

						nodes := nodes + 2;

						WRiteLn('Mid node: ',GetString(mid^, s));
						WriteLN('new node: ',GetString(newNode^, s));
						WriteLn('Updated child node: ',GetString(child^, s));

						break;
					END;
				END
				ELSE
				BEGIN
					child := GetMem(SizeOf(TNode));
					child^.offset := j - 1;
					child^.len := Length(s) - child^.offset;
					child^.next_sibling := cur^.child;
					child^.child := nil;

					cur^.child := child;

					nodes := nodes + 1;

					WriteLn('new child node: ',GetString(child^, s));
					break;
				END;
			END;
		END;
		result.root := root;
		result.s := s;
		result.nodes := nodes;
	END;

	FUNCTION GetString(node : TNode; s : String) : String;
	VAR
		i : Integer;
		{result : String;}
	BEGIN
		result := '';
		{WRiteLn('offset: ', node.offset);
		WriteLn('length: ', node.len);
		WriteLN('string: ', s);}
		FOR i := node.offset + 1 TO node.offset + node.len DO
		BEGIN
			result := result + s[i];
		END;
	END;

	{FUNCTION CountNodes(node : Nptr) : Integer;
	{VAR
		result : Integer;
	BEGIN
		result := 0;
		IF node <> nil THEN
		BEGIN
			result := 1 + CountNodes(node^.next_sibling) + CountNodes(node^.child);
		END;
	END;

	FUNCTION NumberLeaves(tree : TSuffixTree) : Integer;
	{VAR
		result : Integer;
	BEGIN
		result := CountNodes(Addr(tree.root));
	END;}

	FUNCTION FindSubstrings(length : Integer; repetitions : Integer; 
		tree : TSuffixTree) : Boolean;
	BEGIN
		result := false;
	END;

BEGIN
END.
