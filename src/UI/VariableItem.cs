using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CobolAnalyzer.UI
{
    class VariableItem:Item
    {
    
        private int _startChar, _startLine, _endChar, _endLine;

        public VariableItem(int startChar, int startLine, int endChar, int endLine)
        {
            _startChar = startChar;
            _startLine = startLine;
            _endChar = endChar;
            _endLine = endLine;
        }

        public string ShowDetails()
        {
            return "Variable ecc..." + "\n\nstartChar: " + StartChar + "\nstartLine: " + StartLine + "\nendChar: " + EndChar + "\nendLine: " + EndLine;
        }

        public int StartChar
        {
            get { return _startChar; }
        }

        public int StartLine
        {
            get { return _startLine; }
        }

        public int EndChar
        {
            get { return _endChar; }
        }

        public int EndLine
        {
            get { return _endLine; }
        }

        public string Name
        {
            get { return "Lorem ipsum" + new Random().Next(); }
        }

        public FlowType Checkout()
        {
            Random rand = new Random();
            List<VariableItem> curr = new List<VariableItem>();
            int i = rand.Next(9), startChar, startLine;
            curr.Add(this);
            for (; i >= 0; i--)
            {
                startChar = rand.Next(0, 50);
                startLine = rand.Next(1, Form1.MAX_ROWS);
                VariableItem t = new VariableItem(StartChar, startLine, StartChar + rand.Next(1, 10), startLine + rand.Next(1));
                curr.Add(t);
            }
            List<VariableItem> trace = new List<VariableItem>();
            i = rand.Next(10);
            for (; i >= 0; i--)
            {
                startChar = rand.Next(0, 50);
                startLine = rand.Next(1, Form1.MAX_ROWS);
                VariableItem t = new VariableItem(StartChar, startLine, StartChar + rand.Next(1, 10), startLine + rand.Next(1));
                trace.Add(t);
            }
            startChar = rand.Next(0, 50);
            startLine = rand.Next(1, Form1.MAX_ROWS);
            return new FlowType(this, curr, trace);
        }
    }
}
