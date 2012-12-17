using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CobolAnalyzer.UI
{
    interface Item
    {

        string ShowDetails();

        int StartChar { get; }

        int StartLine { get; }

        int EndChar { get; }

        int EndLine { get; }

        string Name { get; }

        FlowType Checkout();

    }
}
