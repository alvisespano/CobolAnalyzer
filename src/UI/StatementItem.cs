﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CobolAnalyzer.UI
{
    class StatementItem:Item
    {
    
        private int _startChar, _startLine, _endChar, _endLine;

        public StatementItem(int startChar, int startLine, int endChar, int endLine)
        {
            _startChar = startChar;
            _startLine = startLine;
            _endChar = endChar;
            _endLine = endLine;
        }

        public string ShowDetails()
        {
            return "Statement Item ecc..." + "\n\nstartChar: " + StartChar + "\nstartLine: " + StartLine + "\nendChar: " + EndChar + "\nendLine: " + EndLine;
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
            get { return "Lorem ipsum"; } 
        }

        public FlowType Checkout()
        {
            return null;
        }
    }
}
