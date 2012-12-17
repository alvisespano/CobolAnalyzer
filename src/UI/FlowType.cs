using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CobolAnalyzer.UI
{
    class FlowType
    {

        public FlowType(VariableItem original, List<VariableItem> current, List<VariableItem> trace)
        {
            _original = original;
            _trace = trace;
            _current = current;
        }

        private VariableItem _original;

        public VariableItem Original
        {
            get { return _original; }
        }

        private List<VariableItem> _current;

        public List<VariableItem> Current
        {
            get { return _current; }
        }

        private List<VariableItem> _trace;

        public List<VariableItem> Trace
        {
            get { return _trace; }
        }


    }
}
