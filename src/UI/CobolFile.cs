using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using CobolAnalyzer.Engine;
using System.Windows.Forms;

namespace CobolAnalyzer.UI
{
    class CobolFile : SourceFile 
    {
        private string _fileName;
        private string _ILText;
        private List<String> _sourceFile;
        private int rowNum;
        private Interop.Cobol.Program prg;
        private int _index;


        public CobolFile(String FileName)
        {
            this._fileName = FileName;
            _readFile();
            rowNum = _sourceFile.Count;
            _parse();
            _index = 0;
        }

        public string FileName
        {
            get
            {
                return _fileName;
            }
            set
            {
                _fileName = value;
                _parse();
            }
        }

        private void _readFile()
        {
            string line;
            int i = 0;
            _sourceFile = new List<string>();
            rowNum = 0;
            StreamReader reader = new StreamReader(_fileName);
            try
            {
                line = reader.ReadLine();
                while (line != null)
                {
                    _sourceFile.Insert(i++, line);
                    line = reader.ReadLine();
                }
            }
            catch (Exception)
            {
                Close();
            }
            finally
            {
                reader.Close();
                Dispose(reader);
            }
        }

        public void Close()
        {
            _fileName = "";
            _ILText = "";
            _sourceFile = null;
            _index = 0;
        }        

        private void _parse()
        {
            prg = new Interop.Cobol.Program(_fileName);
            _ILText = prg.PrettyIdeal();
            _index = 0;
        }

        public void Typecheck()
        {
            prg.Typecheck();
            _ILText = prg.PrettyIdealAnnotated().Value;
        }

        private Absyn.Cobol.row_list _rowTypes
        {
            get
            {
                return prg.Cobol.rows;
            }
        }

        public Row NextRow
        {
            get
            {
                if (_index < RowCount)
                {
                    _index++;
                    return new CobolRow(_sourceFile[_index - 1], _rowTypes[_index - 1]);
                }
                else
                { throw new NotImplementedException(); }
            }
        }

        public Item NextItem
        {
            get { throw new NotImplementedException(); }
        }

        public Item GetItemFromCharPos(int rowNumber, int charPosition)
        {

            
            Random rand = new Random(DateTime.Now.Millisecond);
            switch (rand.Next(0))
            {
                case 0: return new VariableItem(charPosition - rand.Next(5), rowNumber, charPosition + rand.Next(3), rowNumber + rand.Next(1));
                case 1: return new CommentItem(0, rowNumber - rand.Next(1), rand.Next(30), rowNumber + rand.Next(2));
                case 2: return new StatementItem(charPosition - rand.Next(5), rowNumber, charPosition + rand.Next(3), rowNumber + rand.Next(1));
                case 3: return new UnknownItem(0, rowNumber, charPosition + rand.Next(30), rowNumber + rand.Next(1));
                default: return null;
            }
        }

        public bool hasRows { get { return _index < RowCount; } }

        public Row RowAt(int index)
        {
            return (index < RowCount ? new CobolRow(_sourceFile[index], _rowTypes[index]) : null);
        }

        public int RowCount
        {
            get { return (_sourceFile != null ? rowNum : 0); }
        }

        public string ILText
        {
            get { return _ILText; }
        }


        private void Dispose(object o)
        {
            try
            {
                System.Runtime.InteropServices.Marshal.FinalReleaseComObject(o);
                GC.Collect();
                GC.WaitForPendingFinalizers();
            }
            catch (Exception) { }
        }
    }
}
