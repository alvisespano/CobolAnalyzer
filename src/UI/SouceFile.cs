using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace CobolAnalyzer.UI
{
    interface SourceFile
    {
       
         String FileName { get; set; }

         void Close();

         void Typecheck();

         Row NextRow { get; }

         Item NextItem { get; }

         Item GetItemFromCharPos(int RowNumber, int CharPosition);

         bool hasRows { get; }

         Row RowAt(int index);

         int RowCount { get; }

         string ILText { get; }

    }
}
