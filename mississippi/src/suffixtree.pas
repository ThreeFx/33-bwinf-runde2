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

		ListPtr = ^TList;

		TList = RECORD
			start : NodePtr;
			node : NodePtr;
			rep : Integer;
			next : ListPtr;
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
		WriteLn('[LeavesBelow] Starting execution');
		result := 1;
		IF node <> nil THEN
		BEGIN
			Write('[LeavesBelow] Checking for child... ');
			IF node^.child <> nil THEN
			BEGIN
				WriteLn('child exists, continuing in child');
				result := LeavesBelow(node^.child);
			END
			ELSE
			BEGIN
				WriteLn('no child');
			END;
			WRite('[LeavesBelow] Checking for sibling... ');
			IF node^.next_sibling <> nil THEN
			BEGIN
				WRiteln('sibling exists');
				result := result + LeavesBelow(node^.next_sibling);
			END
			ELSE
			BEGIN
				WRiteLn('no sibling');
			END;
		END;
	END;

	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	BEGIN
		result := LeavesBelow(tree.root);
	END;

	FUNCTION Add(start, node : NodePtr; rep : Integer; list : ListPtr) : ListPtr;
	BEGIN
		result := list;
		IF (start <> nil) AND (node <> nil) THEN
		BEGIN
			result := GetMem(SizeOf(ListPtr));
			result^.start := start;
			result^.node := node;
			result^.rep := rep;
			result^.next := list;
		END;
	END;

	FUNCTION AddCollection(newlist, list : ListPtr) : ListPtr;
	BEGIN
		result := list;
		IF (newList <> nil) AND (result <> nil) THEN
		BEGIN
			WHILE newList <> nil DO
			BEGIN
				result := Add(newList^.start, newList^.node, newList^.rep, result);
				newList := newList^.next;
			END;
		END
		ELSE IF list = nil THEN
		BEGIN
			result := newList;
		END;
	END;

	FUNCTION FindSubstringsInNode(len, rep : Integer; branch, node : NodePtr; curLen : Integer; s : String) : ListPtr;
	BEGIN
		result := nil;

		IF node <> nil THEN
		BEGIN
			{
			WriteLn('[FindSubstringsInNode] Beginning search at: ',GetString(node, s));
			}

			node := node^.child;

			WHILE node <> nil DO
			BEGIN
				{
				WriteLn('[FindSubstringsInNode] Node #',Integer(node),': ',GetString(node, s),
				        ' has ',LeavesBelow(node^.child),
				        ' leaves; current length ',curLen + node^.len);
				}

				IF ((curLen + node^.len) >= len) AND (LeavesBelow(node^.child) >= rep) THEN
				BEGIN
					result := Add(branch, node, LeavesBelow(node^.child), result);
				END
				ELSE
				BEGIN
					result := AddCollection(FindSubstringsInNode(len, rep, branch, node^.child, curLen + node^.len, s), result);
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
			WRitelN('[GetStringBetween] Beginning print of ', s);
			WriteLn('[GetStringBetween] i beginning: ',top^.offset + 1,' goal beginning: ',goal^.offset + goal^.len);
			FOR i := top^.offset + 1 TO goal^.offset + goal^.len DO
			BEGIN
				WriteLn('[GetStringBetween] i: ',i,' length s: ',Length(s));
				WriteLn('[GetStringBetween] s[i]: ',s[i]);
				WriteLn('[GetStringBetween] result: ',result);
				result := result + s[i];
				WRiteLn('[GetStringBetween] sucessfully added ',s[i]);
			END;
			WriteLn('[GetStringBetween] exited successfully');
		END;
	END;

	PROCEDURE PrintFoundStrings(list : ListPtr; s : String);
	BEGIN
		IF list <> nil THEN
		BEGIN
			WHILE list <> nil DO
			BEGIN
				WriteLn(GetStringBetween(list^.start, list^.node, s), ' (', list^.rep, ')');
				list := list^.next;
			END;
		END;
	END;

	PROCEDURE FindSubstrings(len, rep : Integer; tree : TSuffixTree);
	VAR
		child : NodePtr;
		list : ListPtr;
	BEGIN
		list := nil;
		child := tree.root^.child;

		WHILE child <> nil DO
		BEGIN
			list := AddCollection(FindSubstringsInNode(len, rep, child, child, child^.len, tree.s), list);
			child := child^.next_sibling;
		END;

		WRiteLn(list <> nil);

		PrintFoundStrings(list, tree.s);
	END;

BEGIN
END.
