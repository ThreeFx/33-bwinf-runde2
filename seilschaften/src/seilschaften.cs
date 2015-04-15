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
            StreamReader sr = new StreamReader(args[0]);

            Console.Write("Reading file... ");
            int d = int.Parse(sr.ReadLine());
            Weight[] weights = sr.ReadToEnd().Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries).Select(Weight.Parse).ToArray();
            Console.WriteLine("done");

            ProblemState problem = new ProblemState(d, weights);
            problem.SolveBFS();

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }

    class ProblemState
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

        public void SolveBFS()
        {
            Queue<ProblemState> queue = new Queue<ProblemState>();
            List<ProblemState> visitedStates = new List<ProblemState>();
            queue.Enqueue(this);
            visitedStates.Add(this);
            ProblemState result = null;

            int moves = 0;

            Stopwatch sw = new Stopwatch();
            sw.Start();

            while (queue.Count != 0 && result == null)
            {
                moves++;
                List<ProblemState> temp = new List<ProblemState>();

                while (queue.Count != 0)
                {
                    ProblemState current = queue.Dequeue();

                    foreach (ProblemState p in current.AllPossibleMoves())
                    {
                        if (!visitedStates.Contains(p))
                        {
                            temp.Add(p);
                            visitedStates.Add(p);

                            if (p.IsFinished())
                            {
                                result = p;
                            }
                        }
                    }
                }
                foreach (ProblemState newState in temp)
                {
                    queue.Enqueue(newState);
                }
            }

            sw.Stop();

            if (result != null)
            {
                result.PrintSolution(moves);
                Console.WriteLine("Total moves: {0}", moves);
            }
            else
            {
                Console.WriteLine("No possible solution!");
            }
            Console.WriteLine("Program took {0}ms", sw.ElapsedMilliseconds);
        }

        public bool IsFinished()
        {
            return !Upper.Any(x => x.IsPerson);
        }

        public ProblemState Move(Weight[] fromUpper, Weight[] fromLower)
        {
            ProblemState result = new ProblemState(this);

            result.Upper.ExceptWith(fromUpper);
            result.Lower.ExceptWith(fromLower);

            result.Upper.UnionWith(fromLower);
            result.Lower.UnionWith(fromUpper);

            return result;
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

        public IEnumerable<ProblemState> AllPossibleMoves()
        {
            List<ProblemState> result = new List<ProblemState>();

            Weight[][] possibleUpper = Powerset(this.Upper);
            Weight[][] possibleLower = Powerset(this.Lower);

            for (int i = 1; i < possibleUpper.Length; i++)
            {
                for (int j = 0; j < possibleLower.Length; j++)
                {
                    int diff = possibleUpper[i].Sum(x => x.ActualWeight) - possibleLower[j].Sum(x => x.ActualWeight);
                    if (diff > 0 && IsValid(possibleLower[j]) && (diff <= d || (possibleUpper[i].All(x => !x.IsPerson) && possibleLower[j].All(x => !x.IsPerson))))
                    {
                        result.Add(this.Move(possibleUpper[i], possibleLower[j]));
                    }
                    //else if (diff < 0)
                    //{
                    //    break;
                    //}
                }
            }
            return result;
        }

        private bool IsValid(Weight[] weights)
        {
            return weights.Length == 0 || this.Lower.Any(x => x.IsPerson) || (this.parent != null ? this.parent.Upper.IsSupersetOf(weights) : false);
        }

        public void PrintSolution(int move)
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

        private static Weight[][] Powerset(HashSet<Weight> set)
        {
            Weight[] seq = set.ToArray();
            Weight[][] powerSet = new Weight[1 << seq.Length][];
            powerSet[0] = new Weight[0];
            for (int i = 0; i < seq.Length; i++)
            {
                var cur = seq[i];
                int count = 1 << i;
                for (int j = 0; j < count; j++)
                {
                    Weight[] source = powerSet[j];
                    Weight[] destination = powerSet[count + j] = new Weight[source.Length + 1];
                    for (int q = 0; q < source.Length; q++)
                        destination[q] = source[q];
                    destination[source.Length] = cur;
                }
            }
            return powerSet;
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
