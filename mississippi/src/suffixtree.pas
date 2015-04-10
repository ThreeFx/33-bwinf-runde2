{ (c) Ben Fiedler 2015 }

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
		// Zu Anfang ist keine Zeichenkette vorhanden
		result := '';

		// Wenn der Knoten existiert
		IF node <> nil THEN
		BEGIN
			// Sollen alle Zeichen im Bereich der Kante
			FOR i := node^.offset + 1 TO node^.offset + node^.len DO
			BEGIN
				// Zu einer Zeichenkette zusammengefügt werden
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
		// Es wird davon ausgegegangen, dass kein
		// Knoten mit dem Buchstaben existiert
		result := nil;
		WHILE node <> nil DO
		BEGIN
			// Solange es noch Nachbarn gibt:

			// Wenn der Anfangsbuchstabe der KAnte mit dem gewünschten
			// Anfangsbuchstaben übereinstimmt
			IF s[node^.offset + 1] = c THEN
			BEGIN
				// Gebe diesen Knoten zurück
				result := node;
				exit;
			END;

			// Ansonsten: Suche weiter
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
		s := s + '$'; //$ Das ist für mein Vim Syntax Highlighting

		// Erstellen eines Knotens mit der Information des gesamten Strings
		cur := GetMem(SizeOf(TNode));
		cur^.offset := 0;
		cur^.len := Length(s);
		cur^.next_sibling := nil;
		cur^.child := nil;

		// Erstellung des Wurzelknotens
		root := GetMem(SizeOf(TNode));
		root^.offset := 0;
		root^.len := 0;
		root^.child := nil;
		root^.next_sibling := nil;

		// Anfügen des Strings an die Wurzel
		AddChild(root, cur);
		nodes := 2;

		// Für alle verbleibenden Suffixe
		FOR i := 2 TO Length(s) DO
		BEGIN
			// Beginne an der Wurzel
			cur := root;
			j := i;

			WHILE j <= Length(s) DO
			BEGIN
				// Bestimme, ob bereits eine Kante mit dem Buchstaben
				// von dem aktuellen Knoten ausgeht
				child := GetNodeAt(cur^.child, s, s[j]);

				// Wenn dies der Fall ist
				IF child <> nil THEN
				BEGIN
					offset := child^.offset;
					len := child^.len;

					// Laufe den String Schritt für Schritt solange
					// wie er identisch mit der Kante ist ab
					k := j + 1;
					WHILE (k - j < len) AND (s[k] = s[offset + 1 + k - j]) DO
					BEGIN
						Inc(k);
					END;

					// Wenn die Kante vollständig durchlaufen wurde
					IF (k - j) = len THEN
					BEGIN
						// Führe dasselbe Verfahren am nächsten Knoten durch
						cur := child;
						j := k;
					END
					ELSE
					BEGIN
						// Ansonsten sind wir mitten in der Kante abgefallen und
						// müssen diese teilen

						// Erstelle eine neue Kante mit dem neuen Rest, der noch
						// eingefügt werden muss
						newNode := GetMem(SizeOf(TNode));
						newNode^.offset := k - 1;
						newNode^.len := Length(s) - newNode^.offset;
						newNode^.child := nil;
						newNode^.next_sibling := nil;

						// Erstelle einen neuen Knoten, an dem die Abzweigung
						// verläuft
						mid := GetMem(SizeOf(TNode));
						mid^.offset := child^.offset;
						mid^.len := k - j;
						mid^.child := nil;
						mid^.next_sibling := nil;

						// Passe die existierende Kante an
						child^.offset := child^.offset + (k - j);
						child^.len := child^.len - (k - j);

						// Füge den mittleren Knoten in der Mitte ein
						ReplaceChildWith(cur, child, mid);

						// Füge die beiden Suffixreste an der neu erstellten Verzweigung an
						AddChild(mid, newNode);
						AddChild(mid, child);

						nodes := nodes + 2;
						break;
					END;
				END
				ELSE
				BEGIN
					// Wenn noch keine Kante mit dem aktuellen Buchstaben beginnt,
					// füge eine neue Kante an dem aktuellen Knoten an
					child := GetMem(SizeOf(TNode));
					child^.offset := j - 1;
					child^.len := Length(s) - child^.offset;
					child^.child := nil;
					child^.next_sibling := nil;
					AddChild(cur, child);

					nodes := nodes + 1;
					break;
				END;
			END;
		END;
		// Schließlich fehlt nur noch die Zweisung der Felder des Suffixbaums
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

	FUNCTION Add(nodelist : NodeListPtr; l, k : Integer; list : ResultListPtr; s : String) : ResultListPtr; OVERLOAD;
	VAR
		curNode : ResultListPtr;
	BEGIN
		// Das Ergebnis sei die Ursprungsliste
		result := list;

		// Wenn die einzufügende Teilkette nicht nil ist
		IF nodelist <> nil THEN
		BEGIN
			curNode := list;

			// Solange die Ergebnisliste nicht leer ist
			WHILE curNode <> nil DO
			BEGIN
				// Wenn die Anzahl der Wiederholungen der einzufügenden
				// Zeichenkette und der aktuellen gleich sind.
				IF curNode^.rep = k THEN
				BEGIN
					// Wenn die aktuell betrachtete Teilkette Bestandteil der
					// einzufügenden ist
					IF Pos(GetStringFrom(curNode^.nodes, s), GetStringFrom(nodelist, s)) > 0 THEN
					BEGIN
						// Ersetze die aktuelle Teilkette mit der einzufügenden,
						// da die einzufügende maximaler ist
						curNode^.nodes := nodelist;
						curNode^.len := l;
						exit;
					END
					// Ansonsten wenn die einzufügende Teilkette Bestandteil
					// der aktuell betrachtenden ist
					ELSE IF Pos(GetStringFrom(nodelist, s), GetStringFrom(curNode^.nodes, s)) > 0 THEN
					BEGIN
						// Breche den Vorgang ab, da die maximale Teilkette
						// bereits vorhanden ist
						exit;
					END;
				END;

				// Wiederholde dasselbe für das nächste Element
				curNode := curNode^.next;
			END;

			// Ansonsten wurde eine neue Teilkette gefunden und eingefügt
			result := GetMem(SizeOf(TResultList));
			result^.nodes := nodelist;
			result^.len := l;
			result^.rep := k;
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

	// Übergeben werden: Die Mindestlänge, Mindesthäufigkeit, aktueller Pfad
	FUNCTION FindSubstringsInNode(l, k : Integer; path : NodeListPtr;
	// Der aktuelle Knoten, die aktuelle Länge des Pfages und die Zeichenkette,
	// aus der der Baum erzeugt wurde
		node : NodePtr; len : Integer; s : String) : ResultListPtr;
	BEGIN
		// Es wird davon ausgegangen, dass keine
		// Zeichenketten den Kriterien entsprechen
		result := nil;

		// Solange es noch Nachbarn gibt:
		WHILE node <> nil DO
		BEGIN
			// Wenn sie mindestens l Zeichen lang ist und k-mal vorkommt
			IF ((len + node^.len) >= l) AND (LeavesBelow(node) >= k) THEN
			BEGIN
				// Füge sie dem Ergebnis hinzu
				result := Add(Add(node, path), node^.len + len, LeavesBelow(node), result, s);
			END;

			// Füge dem Ergebnis alle gefundenen Teilzeichenketten des Kindes
			// des aktuellen Knotens hinzu
			result := AddCollection(
						FindSubstringsInNode(l, k, Add(node, path), node^.child, node^.len + len, s),
						result, s);

			// Führe dasselbe Verfahren für den nächsten Nachbarn durch
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
