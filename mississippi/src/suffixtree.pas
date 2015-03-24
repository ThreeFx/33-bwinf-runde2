UNIT SuffixTree;

INTERFACE

	TYPE
		NodePtr = ^TNode;
		NodeListPtr = ^TNodeList;
		ResultListPtr = ^TResultList;

		TNode = RECORD
			offset       : Integer;
			len          : Integer;
			next_sibling : NodePtr;
			child        : NodePtr;
		END;

		TNodeList = RECORD
			node : NodePtr;
			next : NodeListPtr;
		END;

		TResultList = RECORD
			nodes : NodeListPtr;
			len : Integer;
			rep : Integer;
			next : ResultListPtr;
		END;

		TSuffixTree = RECORD
			root : NodePtr;
			s    : String;
			nodes : Integer;
		END;

	FUNCTION CreateSuffixTree(s : String) : TSuffixTree;
	PROCEDURE DisposeTree(tree : TSuffixTree);
	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	PROCEDURE FindSubstrings(len, rep : Integer; tree : TSuffixTree);

IMPLEMENTATION USES SysUtils;

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
		END;
	END;

	FUNCTION GetStringFrom(list : NodeListPtr; s : String) : String;
	BEGIN
		result := '';
		WHILE list <> nil DO
		BEGIN
			result := GetString(list^.node, s) + result;
			list := list^.next;
		END;
	END;

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
			WriteLn('[ReplaceChild]: An error occured; An argument was null');
		END;
	END;

	FUNCTION CreateSuffixTree(s : String) : TSuffixTree;
	VAR
		i, j, k, len, offset, nodes : Integer;
		root, cur, child, mid, newNode : NodePtr;
	BEGIN
		s := s + '$';
		{
		WriteLn('[CreateSuffixTree] Input size: ',Length(s));
		}

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

	FUNCTION LeavesAt(node : NodePtr) : Integer;
	BEGIN
		result := 1;
		IF node <> nil THEN
		BEGIN
			IF node^.child <> nil THEN
			BEGIN
				result := LeavesAt(node^.child);
			END;
			IF node^.next_sibling <> nil THEN
			BEGIN
				result := result + LeavesAt(node^.next_sibling);
			END;
		END;
	END;

	FUNCTION LeavesBelow(node : NodePtr) : Integer;
	BEGIN
		result := 1;
		IF node <> nil THEN
		BEGIN
			result := LeavesAt(node^.child);
		END;
	END;

	FUNCTION CountLeaves(tree : TSuffixTree) : Integer;
	BEGIN
		result := LeavesAt(tree.root);
	END;

	FUNCTION Add(node : NodePtr; list : NodeListPtr) : NodeListPtr; OVERLOAD;
	BEGIN
		result := list;
		IF node <> nil THEN
		BEGIN
			result := GetMem(SizeOf(TNodeList));
			result^.node := node;
			result^.next := list;
		END;
	END;

	FUNCTION Add(nodelist : NodeListPtr; len, rep : Integer; list : ResultListPtr; s : String) : ResultListPtr; OVERLOAD;
	VAR
		curNode : ResultListPtr;
	BEGIN
		result := list;
		IF nodelist <> nil THEN
		BEGIN
			curNode := list;
			WHILE curNode <> nil DO
			BEGIN
				IF curNode^.rep = rep THEN
				BEGIN
					IF Pos(GetStringFrom(curNode^.nodes, s), GetStringFrom(nodelist, s)) > 0 THEN
					BEGIN
						curNode^.nodes := nodelist;
						curNode^.len := len;
						exit;
					END
					ELSE IF Pos(GetStringFrom(nodelist, s), GetStringFrom(curNode^.nodes, s)) > 0 THEN
					BEGIN
						exit;
					END;
				END;

				curNode := curNode^.next;
			END;

			result := GetMem(SizeOf(TResultList));
			result^.nodes := nodelist;
			result^.len := len;
			result^.rep := rep;
			result^.next := list;
		END;
	END;

	FUNCTION AddCollection(newlist, list : ResultListPtr; s : String) : ResultListPtr;
	BEGIN
		result := list;
		IF result <> nil THEN
		BEGIN
			WHILE newList <> nil DO
			BEGIN
				result := Add(newList^.nodes, newList^.len, newlist^.rep, result, s);
				newList := newList^.next;
			END;
		END
		ELSE
		BEGIN
			result := newList;
		END;
	END;

	FUNCTION FindSubstringsInNode(len, rep : Integer; ilist : NodeListPtr;
		node : NodePtr; curLen : Integer; s : String) : ResultListPtr;
	BEGIN
		result := nil;

		WHILE node <> nil DO
		BEGIN
			{
			WriteLn('[FindSubstringsInNode] Node #',Integer(node),': ',GetString(node, s),
				    ' has ',LeavesBelow(node),
				    ' leaves; current length ',curLen + node^.len);
			}

			IF ((curLen + node^.len) >= len) AND (LeavesBelow(node) >= rep) THEN
			BEGIN
				result := Add(Add(node, ilist), node^.len + curLen, LeavesBelow(node), result, s);
			END;

			result := AddCollection(
			            FindSubstringsInNode(len, rep, Add(node, ilist), node^.child, node^.len + curlen, s),
			            result, s);

			node := node^.next_sibling;
		END;
	END;

	PROCEDURE PrintFoundStrings(list : ResultListPtr; s : String);
	BEGIN
		WHILE list <> nil DO
		BEGIN
			WriteLn(GetStringFrom(list^.nodes, s), ' (', list^.rep, ')');
			list := list^.next;
		END;
	END;

	PROCEDURE DisposeResultList(list : ResultListPtr); FORWARD;

	PROCEDURE FindSubstrings(len, rep : Integer; tree : TSuffixTree);
	VAR
		list : ResultListPtr;
	BEGIN
		list := FindSubstringsInNode(len, rep, nil, tree.root, 0, tree.s);

		PrintFoundStrings(list, tree.s);
	END;

	PROCEDURE DisposeNodeList(nlist : NodeListPtr);
	BEGIN
		IF nList <> nil THEN
		BEGIN
			DisposeNodeList(nList^.next);
			FreeMem(nList);
		END;
	END;

	PROCEDURE DisposeResultList(list : ResultListPtr);
	BEGIN
		IF list <> nil THEN
		BEGIN
			DisposeNodeList(list^.nodes);
			DisposeResultList(list^.next);
			FreeMem(list);
		END;
	END;

	PROCEDURE DisposeNode(node : NodePtr);
	BEGIN
		IF node <> nil THEN
		BEGIN
			DisposeNode(node^.child);
			DisposeNode(node^.next_sibling);
			FreeMem(node);
		END;
	END;

	PROCEDURE DisposeTree(tree : TSuffixTree);
	BEGIN
		DisposeNode(tree.root);
	END;

BEGIN
END.
