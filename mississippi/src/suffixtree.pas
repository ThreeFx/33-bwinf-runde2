UNIT SuffixTree;

INTERFACE

	TYPE
		NodePtr = ^TNode;

		TNode = RECORD
			offset       : Integer;
			len          : Integer;
			next_sibling : NodePtr;
			child        : NodePtr;
		END;

		TSuffixTree = RECORD
			root : NodePtr;
			s    : String;
			nodes : Integer;
		END;

		ResultPtr = ^TResult;

		TResult = RECORD
			node : NodePtr;
			rep : Integer;
			next : ResultPtr;
		END;

	FUNCTION CreateSuffixTree(s : String) : TSuffixTree;
	FUNCTION GetString(node : NodePtr; s : String) : String;
	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	PROCEDURE FindSubstrings(len, rep : Integer; tree : TSuffixTree);

IMPLEMENTATION

	FUNCTION GetNodeAt(node : NodePtr; s : String; c : Char) : NodePtr;
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

	FUNCTION NumberChildren(node : NodePtr) : Integer;
	BEGIN
		node := node^.child;
		result := 0;
		WHILE node <> nil DO
		BEGIN
			Inc(result);
			node := node^.next_sibling;
		END;
	END;

	PROCEDURE PrintChildren(node : NodePtr; s : String);
	BEGIN
		IF node <> nil THEN
		BEGIN
			node := node^.child;
			WHILE node <> nil DO
			BEGIN
				WriteLn(GetString(node, s), ' id: ', Integer(node));
				node := node^.next_sibling;
			END;
		END;
	END;

	PROCEDURE AddChild(parent, child : NodePtr);
	BEGIN
		IF (parent <> nil) AND (child <> nil) THEN
		BEGIN
			child^.next_sibling := parent^.child;
			parent^.child := child;
		END;
	END;

	PROCEDURE ReplaceChildWith(node, oldChild, newChild : NodePtr);
	VAR
		currentChild, temp : NodePtr;
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
		root, cur, child, mid, newNode : NodePtr;
	BEGIN
		s := s + '$';
		WriteLn('[CreateSuffixTree] Input size: ',Length(s));

		cur := GetMem(SizeOf(TNode));
		cur^.offset := 0;
		cur^.len := Length(s);
		cur^.next_sibling := nil;
		cur^.child := nil;

		{
		WriteLn('[CreateSuffixTree] Inserted full string: ',GetString(cur, s));
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
						WriteLn('[CreateSuffixTree] Parent node: ', GetString(cur, s));
						WriteLn('[CreateSuffixTree] Current node: ',GetString(child, s));
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
						WriteLn('[CreateSuffixTree] Mid node: ', GetString(mid, s));
						WriteLn('[CreateSuffixTree] New node: ', GetString(newNode, s));
						WriteLn('[CreateSuffixTree] Update child node: ', GetString(child, s));
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
					WriteLn('[CreateSuffixTree] Parent node: ',GetString(cur, s));
					WriteLn('[CreateSuffixTree] Inserted new child: ',GetString(child, s));
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

	FUNCTION GetString(node : NodePtr; s : String) : String;
	VAR
		i : Integer;
	BEGIN
		result := '';
		IF node <> nil THEN
		BEGIN
			FOR i := node^.offset + 1 TO node^.offset + node^.len DO
			BEGIN
				result := result + s[i];
			END;
			IF result = '' THEN
			BEGIN
				result := 'root node';
			END;
		END;
	END;

	FUNCTION LeavesBelow(node : NodePtr) : Integer;
	BEGIN
		result := 1;
		IF node <> nil THEN
		BEGIN
			IF node^.child <> nil THEN
			BEGIN
				result := LeavesBelow(node^.child);
			END;
			IF node^.next_sibling <> nil THEN
			BEGIN
				result := result + LeavesBelow(node^.next_sibling);
			END;
		END;
	END;

	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	BEGIN
		result := LeavesBelow(tree.root);
	END;

	FUNCTION CreateNew : ResultPtr;
	BEGIN
		result := GetMem(SizeOf(ResultPtr));
		result^.node := nil;
		result^.next := nil;
	END;

	PROCEDURE Add(node : NodePtr; rep : Integer; oldRes : ResultPtr);
	VAR
		newRes : ResultPtr;
	BEGIN
		IF (node <> nil) AND (oldRes <> nil) THEN
		BEGIN
			newRes := GetMem(SizeOf(ResultPtr));
			newRes^.node := node;
			newRes^.rep := rep;
			newRes^.next := oldRes^.next;

			oldRes^.next := newRes;
		END;
	END;

	PROCEDURE AddCollection(c, oldRes : ResultPtr);
	BEGIN
		IF (c <> nil) AND (oldRes <> nil) THEN
		BEGIN
			WHILE c <> nil DO
			BEGIN
				Add(c^.node, c^.rep, oldRes);
				c := c^.next;
			END;
		END;
	END;

	FUNCTION FindSubstringsInNode(len, rep : Integer; node : NodePtr; currentDepth : Integer; s : String) : ResultPtr;
	BEGIN
		result := CreateNew;
		IF node <> nil THEN
		BEGIN
			WriteLn('[FindSubstringsInNode] Beginning search');
			node := node^.child;

			WHILE node <> nil DO
			BEGIN
				WriteLn('[FindSubstringsInNode] Node #',Integer(node),': ',GetString(node, s),
				        ' has ',LeavesBelow(node^.child),
				        ' leaves; current length ',currentDepth + node^.len);

				IF (currentDepth + node^.len >= len) AND (LeavesBelow(node^.child) >= rep) THEN
				BEGIN
					WriteLn('[FindSubstringsInNode] Found node: ',GetString(node, s));
					Add(node, LeavesBelow(node^.child), result);
				END
				ELSE
				BEGIN
					AddCollection(FindSubstringsInNode(len, rep, node^.child, currentDepth + node^.len, s), result);
				END;

				node := node^.next_sibling;
			END;
		END;
	END;

	FUNCTION GetStringBetween(top, goal : NodePtr; s : String) : String;
	VAR
		i : Integer;
	BEGIN
		result := '';

		IF (top <> nil) AND (goal <> nil) THEN
		BEGIN
			FOR i := top^.offset + 1 TO goal^.offset + goal^.len DO
			BEGIN
				result := result + s[i];
			END;
		END;
	END;

	PROCEDURE PrintFoundStrings(start : NodePtr; res : ResultPtr; s : String);
	BEGIN
		IF res <> nil THEN
		BEGIN
			WHILE res <> nil DO
			BEGIN
				WriteLn(GetStringBetween(start, res^.node, s), ' (', res^.rep, ')');
				res := res^.next;
			END;
		END;
	END;

	PROCEDURE FindSubstrings(len, rep : Integer; tree : TSuffixTree);
	BEGIN
		PrintFoundStrings(tree.root, FindSubstringsInNode(len, rep, tree.root, 0, tree.s), tree.s);
	END;

BEGIN
END.
