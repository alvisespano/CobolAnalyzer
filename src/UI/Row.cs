using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CobolAnalyzer.Engine;

namespace CobolAnalyzer.UI
{
    interface Row
    {

        string SourceRow { get; }

        Absyn.Cobol.row_type Type { get; }

    }
}
