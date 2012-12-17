using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CobolAnalyzer.Engine;

namespace CobolAnalyzer.UI
{
    class CobolRow : Row 
    {
        private string _source;
        private Absyn.Cobol.row_type _type;

        public CobolRow(string source, Absyn.Cobol.row_type type)
        {
            _source = source;
            _type = type;
        }

        public CobolRow()
        { }

        public string SourceRow { get { return _source; } set { _source = value; } }

        public Absyn.Cobol.row_type Type { get { return _type; } set { _type = value; } }
    }
}
