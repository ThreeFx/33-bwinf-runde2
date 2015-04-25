using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using System.Text;
using System.IO;

namespace Seilschaften
{
    class Program
    {
        static void Main(string[] args)
        {
            ProblemState problem = null;
            try
            {
                problem = ProblemState.ParseFromFile(args[0]);
            }
            catch
            {
                Console.WriteLine("An error occurred, please check if" +
                                  " the specified file was valid.");
            }
            finally
            {
                if (problem != null)
                {
                    BFS bfs = new BFS(problem);
                    bfs.Solve();
                }
            }

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }

    class BFS
    {
        private BFSNode start;

        public BFS(BFSNode start)
        {
            this.start = start;
        }

        public void Solve()
        {
            // Die aktuell zu betrachtenden Zustände
            Queue<BFSNode> queue = new Queue<BFSNode>();
            // Die bereits bestimmten und durchlaufenen Zustände
            List<BFSNode> visitedStates = new List<BFSNode>();
            // Zuerst soll der Anfangszustand betrachtet werden
            queue.Enqueue(start);
            // Dieser gilt auch schon als durchlaufen
            visitedStates.Add(start);

            // Die Lösung, falls die denn existiert
            BFSNode result = null;

            // Die Anzahl der Züge, die man bis zur Ermittlung der Lösung braucht
            int moves = 0;

            // Eine Stoppuhr zum Ermitteln der benötigten Zeit
            Stopwatch sw = new Stopwatch();
            sw.Start();

            // Solange es noch Zustände gibt und noch keine Lösung gefunden wurde
            while (queue.Count != 0 && result == null)
            {
                // Erhöhe den Fahrtenzähler um eine Fahrt
                moves++;

                // Erstelle eine Liste für alle neuen Zustände
                List<BFSNode> temp = new List<BFSNode>();

                // Für alle aktuell zu betrachtenden Zustände
                while (queue.Count != 0 && result == null)
                {
                    // Ermittle den aktuell zu betrachtenden
                    BFSNode current = queue.Dequeue();

                    // Für alle Zustände, die aus diesem folgen können
                    foreach (BFSNode p in current.AllPossibleMoves())
                    {
                        // Wenn er noch nicht vorher betrachtet wurde
                        if (!visitedStates.Contains(p))
                        {
                            // Füge ihn den betrachteten und den neuen Zuständen hinzu
                            temp.Add(p);
                            visitedStates.Add(p);

                            // Wenn er eine Lösung darstellt
                            if (p.IsFinished())
                            {
                                // Setze die Lösung auf diesen Zustand
                                result = p;
                                break;
                            }
                        }
                    }
                }
                foreach (BFSNode newState in temp)
                {
                    queue.Enqueue(newState);
                }
            }

            // Stoppe die Stoppuhr
            sw.Stop();

            // Falls eine Lösung gefunden wurde
            if (result != null)
            {
                // Gebe sie aus
                result.PrintSolution(moves);
                Console.WriteLine("Total moves: {0}", moves);
            }
            // Ansonsten
            else
            {
                // Gibt es keine Lösung
                Console.WriteLine("No possible solution!");
            }
            Console.WriteLine("Program took {0}ms", sw.ElapsedMilliseconds);
        }
    }

    abstract class BFSNode
    {
        public abstract IEnumerable<BFSNode> AllPossibleMoves();
        public abstract bool IsFinished();
        public abstract void PrintSolution(int moves);
    }

    class ProblemState : BFSNode
    {
        public int d
        {
            get;
            private set;
        }
        public HashSet<Weight> Upper
        {
            get;
            private set;
        }
        public HashSet<Weight> Lower
        {
            get;
            private set;
        }
        private ProblemState parent;

        public ProblemState(int d, Weight[] weights, ProblemState parent = null)
        {
            this.d = d;
            this.Upper = new HashSet<Weight>();
            this.Lower = new HashSet<Weight>();
            foreach (Weight w in weights)
            {
                var useless = w.IsUp ? Upper.Add(w) : Lower.Add(w);
            }
            this.parent = parent;
        }

        public ProblemState(ProblemState source)
        {
            this.d = source.d;
            this.Upper = new HashSet<Weight>(source.Upper);
            this.Lower = new HashSet<Weight>(source.Lower);
            this.parent = source;
        }

        public static ProblemState ParseFromFile(string filepath)
        {
            StreamReader sr = new StreamReader(filepath);

            int d = int.Parse(sr.ReadLine());
            Weight[] weights = sr.ReadToEnd()
                     .Split(Environment.NewLine.ToCharArray(),
                            StringSplitOptions.RemoveEmptyEntries)
                     .Select(Weight.Parse)
                     .ToArray();

            return new ProblemState(d, weights);
        }

        public override bool IsFinished()
        {
            return !Upper.Any(x => x.IsPerson);
        }

        public override IEnumerable<BFSNode> AllPossibleMoves()
        {
            // Die Liste aller resultierenden Zustände
            List<ProblemState> result = new List<ProblemState>();

            // Erster Schritt: Bestimmen der Potenzmengen der
            // oberen und unteren Gewichte
            Weight[][] possibleUpper = Powerset(this.Upper);
            Weight[][] possibleLower = Powerset(this.Lower);

            // Für alle nicht-leeren Mengen der Potenzmenge oberen Menge
            for (int i = 1; i < possibleUpper.Length; i++)
            {
                // Für alle Mengen der Potenzmenge der unteren Gewichte
                for (int j = 0; j < possibleLower.Length; j++)
                {
                    // Bestimme die Gewichtsdifferenz der Mengen
                    int diff = possibleUpper[i].Sum(x => x.ActualWeight)
                             - possibleLower[j].Sum(x => x.ActualWeight);
                    // Wenn die Differnz größer als 0 ist und der Randfall Psi
                    // beachtet wird
                    if (diff > 0 && IsValid(possibleLower[j])
                    // Und entweder die Differenz nicht größer als dir Schranke d ist
                    // oder alle transportierten Objekte Gewichte sind.
                        && (diff <= d || (possibleUpper[i].All(x => !x.IsPerson)
                                          && possibleLower[j].All(x => !x.IsPerson))))
                    {
                        // Füge die resultierende Fahrt aus der Gewichteverschiebung
                        // dem Ergebnis hinzu
                        result.Add(this.Move(possibleUpper[i], possibleLower[j]));
                    }
                }
            }
            return result;
        }

        // Die Prüfung des Randfalls Psi:
        // Eine Verschibung von unteren Gewichten ist genau dann valid, wenn
        // (0. Keine Gewichte von unten benötigt werden)
        // 1. Sich eine Person unten befindet, die die Gewichte ein- und ausladen kann
        // 2. Oder die Gewichte, die nach oben bewegt werden sollen gerade erst herunter
        //   gelassen worden sind.
        private bool IsValid(Weight[] weights)
        {
                // Keine Gewichte bewegt werden
            return weights.Length == 0
                // Oder sich unten eine Person befindet
                || this.Lower.Any(x => x.IsPerson)
                // Oder die Gewichte gerade erst herunter gelassen worden sind
                || (this.parent != null ? this.parent.Upper.IsSupersetOf(weights) : false);
        }

        private ProblemState Move(Weight[] fromUpper, Weight[] fromLower)
        {
            // Erstelle einen neuen Zustand aus dem aktuellen.
            ProblemState result = new ProblemState(this);

            // Entferne die nach unten bewegten Gewichte von oben
            result.Upper.ExceptWith(fromUpper);
            // und entferne die nach oben bewegen Gewichte von unten
            result.Lower.ExceptWith(fromLower);

            // Füge den oberen Gewichten die von unten bewegten hinzu
            result.Upper.UnionWith(fromLower);
            // und den unteren die von oben kommenden
            result.Lower.UnionWith(fromUpper);

            return result;
        }

        // Entnommen aus: http://stackoverflow.com/questions/19890781/creating-a-power-set-of-a-sequence/19891145
        private static Weight[][] Powerset(HashSet<Weight> set)
        {
            // Begonnen wird mit der Urprungsliste
            Weight[] seq = set.ToArray();
            // Die Potenzmenge umfasst nacher alle 2^n Elemente
            Weight[][] powerset = new Weight[1 << seq.Length][];
            // Die leere Menge wird als ersten eingefügt
            powerset[0] = new Weight[0];
            // Für jedes Gewicht in der Ursprungsliste
            for (int i = 0; i < seq.Length; i++)
            {
                // Bestimme das aktuelle Gewicht
                Weight cur = seq[i];
                // Bestimme die Anzahl der bereits eingefügten Teilmengen
                int count = 1 << i;
                // Für alle bereits exitierenden Teilmengen
                for (int j = 0; j < count; j++)
                {
                    // Bestimme die Liste, an der das aktuelle Element anzufügen ist
                    Weight[] source = powerset[j];

                    // Füge eine neue Teilmenge bestehend aus der aktuellen Liste
                    // mit dem aktuellen Element in die Potenzmenge ein
                    Weight[] destination = powerset[count + j] = new Weight[source.Length + 1];
                    for (int q = 0; q < source.Length; q++)
                        destination[q] = source[q];
                    destination[source.Length] = cur;
                }
            }
            return powerset;
        }

        public override void PrintSolution(int move)
        {
            if (parent == null)
            {
                Console.WriteLine("Initial state:");
                Console.WriteLine(this.ToString());
                Console.WriteLine("Threshold: {0}", d);
            }
            if (parent != null)
            {
                parent.PrintSolution(move - 1);
                Console.WriteLine("Move #{0}:", move);
                Console.WriteLine("Down: |" + String.Join("|", this.Lower.Intersect(parent.Upper)) + "|");
                Console.WriteLine("Up:   |" + String.Join("|", this.Upper.Intersect(parent.Lower)) + "|");
            }
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("Up:");
            sb.AppendLine("|" + String.Join(",", Upper) + "|");
            sb.AppendLine("Down:");
            sb.Append("|" + String.Join(",", Lower) + "|");
            return sb.ToString();
        }

        public override bool Equals(object obj)
        {
            if (obj is ProblemState)
            {
                return this.Upper.SetEquals(((ProblemState)obj).Upper) && this.Lower.SetEquals(((ProblemState)obj).Lower);
            }
            return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    class Weight
    {
        private static int nextId = 0;

        public bool IsPerson
        {
            get;
            private set;
        }
        public int ActualWeight
        {
            get;
            private set;
        }
        public bool IsUp
        {
            get;
            private set;
        }
        public int ID
        {
            get;
            private set;
        }

        public Weight(bool isPerson, int weight, bool isUp)
        {
            IsPerson = isPerson;
            ActualWeight = weight;
            IsUp = isUp;
            ID = nextId++;
        }

        public static Weight Parse(string s)
        {
            string[] parts = s.Split(' ').ToArray();
            try
            {
                return new Weight(parts[0][0] == 'P', int.Parse(parts[1]), parts[2][0] == '^');
            }
            catch (Exception ex)
            {
                throw new FormatException("Wrong input format: ", ex);
            }
        }

        public override int GetHashCode()
        {
            return ID;
        }

        public override bool Equals(object obj)
        {
            if (obj is Weight)
            {
                return ((Weight)obj).ID == this.ID;
            }
            return base.Equals(obj);
        }

        public override string ToString()
        {
            return (IsPerson ? "P" : "W") + ActualWeight.ToString();
        }
    }
}
