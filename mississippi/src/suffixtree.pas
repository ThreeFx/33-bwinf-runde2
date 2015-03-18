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
			root : NPtr;
			s    : String;
			nodes : Integer;
		END;

	FUNCTION CreateSuffixTree(s : String) : TSuffixTree;
	FUNCTION GetString(node : TNode; s : String) : String;
	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;

IMPLEMENTATION

	FUNCTION GetNodeAt(node : Nptr; s : String; c : Char) : Nptr;
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

	FUNCTION NumberChildren(node : Nptr) : Integer;
	BEGIN
		node := node^.child;
		result := 0;
		WHILE node <> nil DO
		BEGIN
			Inc(result);
			node := node^.next_sibling;
		END;
	END;

	PROCEDURE PrintChildren(node : Nptr; s : String);
	BEGIN
		IF node <> nil THEN
		BEGIN
			node := node^.child;
			WHILE node <> nil DO
			BEGIN
				WriteLn(GetString(node^, s), ' id: ', Integer(node));
				node := node^.next_sibling;
			END;
		END;
	END;

	PROCEDURE AddChild(parent, child : Nptr);
	BEGIN
		IF (parent <> nil) AND (child <> nil) THEN
		BEGIN
			child^.next_sibling := parent^.child;
			parent^.child := child;
		END;
	END;

	PROCEDURE ReplaceChildWith(node, oldChild, newChild : Nptr);
	VAR
		currentChild, temp : Nptr;
	BEGIN
		IF (node <> nil) AND (oldChild <> nil) AND (newChild <> nil) THEN
		BEGIN
			currentChild := node^.child;
			IF currentChild = oldChild THEN
			BEGIN
				node^.child := newChild;
				temp := newChild^.next_sibling;
				newChild^.next_sibling := oldChild^.next_sibling;
				oldChild^.next_sibling := temp;
			END
			ELSE
			BEGIN
				WHILE currentChild <> nil DO
				BEGIN
					IF currentChild^.next_sibling = oldChild THEN
					BEGIN
						currentChild^.next_sibling := newChild;
						temp := newChild^.next_sibling;
						newChild.next_sibling := oldChild.next_sibling;
						oldChild^.next_sibling := temp;
						exit;
					END;
					currentChild := currentChild^.next_sibling;
				END;
			END;
		END
		ELSE
		BEGIN
			WRitelN('[ReplaceChild]: An error occured; An argument was null');
		END;
	END;

	FUNCTION CreateSuffixTree(s : String) : TSuffixTree;
	VAR
		i, j, k, len, offset, nodes : Integer;
		root, cur, child, mid, newNode : Nptr;
	BEGIN
		s := s + '$';
		WriteLn('[CreateSuffixTree] Input size: ',Length(s));

		cur := GetMem(SizeOf(TNode));
		cur^.offset := 0;
		cur^.len := Length(s);
		cur^.next_sibling := nil;
		cur^.child := nil;

		{
		WriteLn('[CreateSuffixTree] Inserted full string: ',GetString(cur^, s));
		}
		root := GetMem(SizeOf(TNode));
		root^.offset := 0;
		root^.len := 0;
		root^.child := nil;
		root^.next_sibling := nil;

		AddChild(root, cur);

		nodes := 2;

		FOR i := 2 TO Length(s) DO
		BEGIN
			cur := root;
			j := i;

			{
			WriteLn('---------------------------------------');
			}

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
						cur := child;
						j := k;
					END
					ELSE
					BEGIN
						{
						WriteLn('[CreateSuffixTree] Parent node: ', GetString(cur^, s));
						WriteLn('[CreateSuffixTree] Current node: ',GetString(child^, s));
						}

						newNode := GetMem(SizeOf(TNode));
						newNode^.offset := k - 1;
						newNode^.len := Length(s) - newNode^.offset;
						newNode^.child := nil;
						newNode^.next_sibling := nil;

						mid := GetMem(SizeOf(TNode));
						mid^.offset := child^.offset;
						mid^.len := k - j;
						mid^.child := nil;
						mid^.next_sibling := nil;

						child^.offset := child^.offset + (k - j);
						child^.len := child^.len - (k - j);

						ReplaceChildWith(cur, child, mid);

						AddChild(mid, newNode);
						AddChild(mid, child);

						{
						WriteLn('[CreateSuffixTree] Mid node: ', GetString(mid^, s));
						WriteLn('[CreateSuffixTree] New node: ', GetString(newNode^, s));
						WriteLn('[CreateSuffixTree] Update child node: ', GetString(child^, s));
						WRiteLn('[CreateSuffixTree] Number children: ',NumberChildren(cur));
						WriteLn('[CreateSuffixTree] Children of current:');
						PrintChildren(cur, s);
						WriteLn('[CreateSuffixTree] Children of mid:');
						PrintChildren(mid, s);
						}

						nodes := nodes + 2;

						break;
					END;
				END
				ELSE
				BEGIN
					child := GetMem(SizeOf(TNode));
					child^.offset := j - 1;
					child^.len := Length(s) - child^.offset;
					child^.child := nil;
					child^.next_sibling := nil;

					AddChild(cur, child);

					{
					WriteLn('[CreateSuffixTree] Parent node: ',GetString(cur^, s));
					WriteLn('[CreateSuffixTree] Inserted new child: ',GetString(child^, s));
					}

					nodes := nodes + 1;

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
	BEGIN
		result := '';
		FOR i := node.offset + 1 TO node.offset + node.len DO
		BEGIN
			result := result + s[i];
		END;
		IF result = '' THEN
		BEGIN
			result := 'root node';
		END;
	END;

	FUNCTION CountLeavess(node : Nptr) : Integer;
	BEGIN
		result := 1;
		IF node^.child <> nil THEN
		BEGIN
			result := CountLeavess(node^.child);
		END;
		IF node^.next_sibling <> nil THEN
		BEGIN
			result := result + CountLeavess(node^.next_sibling);
		END;
	END;

	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	BEGIN
		result := CountLeavess(tree.root);
	END;

	PROCEDURE FindSubstrings(len : Integer; repetitions : Integer;
		tree : TSuffixTree);
	VAR
		depth : Integer;
	BEGIN
		depth := 0;
		WHILE depth <= len DO
		BEGIN
			
		END;

	END;

BEGIN
END.
