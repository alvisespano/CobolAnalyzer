using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Threading;
using System.Runtime.InteropServices; 



namespace CobolAnalyzer.UI
{

    public enum ScrollBarType : uint
    {
        SbHorz = 0,
        SbVert = 1,
        SbCtl = 2,
        SbBoth = 3
    }
    public enum Message : uint
    {
        WmVScroll = 0x0115
    }
    public enum ScrollBarCommands : uint
    {
        ThumbPosition = 4,
        ThumbTrack = 5
    }
    [Flags()]
    public enum ScrollBarInfo : uint
    {
        Range = 0x0001,
        Page = 0x0002,
        Pos = 0x0004,
        DisableNoScroll = 0x0008,
        TrackPos = 0x0010,
        All = (Range | Page | Pos | TrackPos)
    }
    public partial class Form1 : Form
    {
        #region Scroll Handlers constants
        [DllImport("User32.dll")]
        public extern static int SendMessage(IntPtr hWnd, uint msg, UIntPtr wParam, IntPtr lParam);
        [DllImport("User32.dll")]
        public extern static int GetScrollInfo(IntPtr hWnd, int fnBar, ref ScrollInfo lpsi);
        [DllImport("User32.dll")]
        public extern static int GetScrollPos(IntPtr hWnd, int nBar);

        
        [StructLayout(LayoutKind.Sequential)]
        public struct ScrollInfo
        {
            public uint cbSize;
            public uint fMask;
            public int nMin;
            public int nMax;
            public uint nPage;
            public int nPos;
            public int nTrackPos;
        };

        #endregion

        
        List<FlowType> History;
        Color[] colors = new Color[] { Color.Wheat, Color.Tan, Color.LightSalmon, Color.Tomato, Color.Red };
        int historyIndex;

        public static int MAX_ROWS = 0;

        SourceFile _file;
        private int _caretPos;
       
        public Form1()
        {           
            InitializeComponent();
            DoubleBuffered = true;
            History = new List<FlowType>(colors.Length);
            historyIndex = -1;
            this.Show();
            this.Location = new Point(Properties.Settings.Default.locationX, Properties.Settings.Default.locationY);
            this.Size = new System.Drawing.Size(Properties.Settings.Default.sizeX, Properties.Settings.Default.sizeY);
        }

        private void printText()
        {
            StringBuilder original = new StringBuilder();
            txt_DerivedLanguage.Text = "";
            txt_MainLanguage.Text = "";
            StatusProgressBar.Style = ProgressBarStyle.Continuous;
            StatusLabel.Text = "Buffering";
            int tmp = 0, count;
            count = _file.RowCount;
            StatusProgressBar.Maximum = count;
            while (_file.hasRows)
            {
               
                original.Append(_file.NextRow.SourceRow + "\n");
                Application.DoEvents();
                tmp++;
                StatusProgressBar.Value = tmp % count;
            }
            MAX_ROWS = count;


            paintBackColorOriginalRows(original,txt_MainLanguage);
            txt_DerivedLanguage.Text = _file.ILText;
            putStatusDone();
        }

        private void paintBackColorOriginalRows(StringBuilder text, RichTextBox rtb)
        {
            DateTime start = DateTime.Now;
            RichTextBox tmp = new RichTextBox();
            if (text == null || text.Equals(String.Empty))
                tmp.Text = rtb.Text;
            else
                tmp.Text = text.ToString();
            if (tmp.Text.Equals(String.Empty)) return;
            tmp.SelectionStart = 0;
            StatusProgressBar.Style = ProgressBarStyle.Continuous;
            StatusProgressBar.Maximum = _file.RowCount;
            StatusProgressBar.Value = 0;
            StatusLabel.Text = "Printing";

            Row r;
            int selLength = 0;

            Application.DoEvents();
            for (int index = 0; index < _file.RowCount; index++)
            {
                r = _file.RowAt(index);
                selLength = r.SourceRow.Length + 1;

                for (int j = index + 1; j < _file.RowCount && _file.RowAt(j).Type.Equals(r.Type); j++)
                {
                    selLength += _file.RowAt(j).SourceRow.Length + 1;
                    index = j;
                }
                tmp.SelectionLength = selLength;
                tmp.SelectionBackColor = colorForRowType(r.Type);
                tmp.SelectionFont = fontForRowType(r.Type);
                tmp.SelectionStart = tmp.SelectionStart + selLength;

                StatusProgressBar.Value = index;
            }
            
            TimeSpan elapsed = DateTime.Now.Subtract(start);
            System.Console.WriteLine(elapsed);
            rtb.Rtf = tmp.Rtf;
            putStatusDone();
        }

        private Color colorForRowType(CobolAnalyzer.Engine.Absyn.Cobol.row_type type)
        {
            Color res;
            if (type.IsDefault) res = Color.White;
            else if (type.IsDivision) res = Color.Tomato;
            else if (type.IsKnownStatement) res = Color.AntiqueWhite;
            else if (type.IsParagraph) res = Color.Violet;
            else if (type.IsPicture) res = Color.Turquoise;
            else if (type.IsPictureArray) res = Color.Turquoise;
            else if (type.IsRecord) res = Color.Yellow;
            else if (type.IsRecordArray) res = Color.Yellow;
            else if (type.IsUnknownStatement) res = Color.Red;
            else res = Color.White;
            return res;
        }

        private Font fontForRowType(CobolAnalyzer.Engine.Absyn.Cobol.row_type type)
        {
            Font resItalic = new Font(FontFamily.GenericSansSerif, 8.25F, FontStyle.Italic, GraphicsUnit.Point);
            Font resRegular = new Font(FontFamily.GenericSansSerif, 8.25F, FontStyle.Regular, GraphicsUnit.Point);

            if (type.IsDefault) return resRegular;
            else if (type.IsDivision) return resItalic;
            else if (type.IsKnownStatement) return resRegular;
            else if (type.IsParagraph) return resRegular;
            else if (type.IsPicture) return resRegular;
            else if (type.IsPictureArray) return resItalic;
            else if (type.IsRecord) return resRegular;
            else if (type.IsRecordArray) return resItalic;
            else if (type.IsUnknownStatement) return resRegular;
            else return resRegular;
        }

        //private void selezionaTuttiChar(String c)
        //{
        //    int index;
            
            
        //    doubleBufferRtb(txt_MainLanguage, (rtb) =>
        //    {
        //        paintBackColorOriginalRows(null,rtb);
        //        rtb.SelectAll();
        //        rtb.SelectionFont = new Font(rtb.SelectionFont.FontFamily, rtb.SelectionFont.Size, FontStyle.Regular);
        //        for (int i = 0; i < rtb.Text.Length; )
        //        {
        //            index = rtb.Text.Substring(i).IndexOf(c);
        //            if (index >= 0)
        //            {
        //                rtb.SelectionStart = i + index;
        //                rtb.SelectionLength = c.Length;
        //                rtb.SelectionBackColor = Color.Red;
        //                rtb.SelectionFont = new Font(rtb.SelectionFont.FontFamily, rtb.SelectionFont.Size, FontStyle.Bold);
        //                i = i + index + c.Length;
        //            }
        //            else { break; }
        //        }
        //    });
        //}

        #region delegates
        private Action<RichTextBox, Action<RichTextBox>> doubleBufferRtb = (rtb, toWrap) =>
        {
            RichTextBox temp = new RichTextBox();
            temp.Rtf = rtb.Rtf;
            toWrap(temp);
            rtb.Rtf = temp.Rtf;
        };

        #endregion

        private void txt_MainLanguage_DoubleClick(object sender, EventArgs e)
        {
            if (txt_MainLanguage.Text.Equals(String.Empty)) return;
            int pos = _caretPos;
            int rowNum = 0, charPos = 0;
            int posCaret = getScrollPosition(txt_MainLanguage.Handle);
            getZeroBasedRowCharPos(ref rowNum, ref charPos, pos,false);
            Item item = _file.GetItemFromCharPos(rowNum, charPos);
            //txt_Details1.Text = orRows[rowNum].RowType.ToString();
            
            txt_Details1.Text = toStringRowType(_file.RowAt(rowNum).Type);
            txt_Details2.Text = item.Name;
            txt_Details3.Text = item.ShowDetails();

            FlowType actFlowType = item.Checkout();
            if (actFlowType == null) return;
            fillTreeView(actFlowType,null);
            shiftHistory(actFlowType);
            repaintVariablesIntoRTB();
            Scroll(txt_MainLanguage.Handle, posCaret);
            
            
            
        }

        private string toStringRowType(CobolAnalyzer.Engine.Absyn.Cobol.row_type type)
        {
            string res = "";

            if (type.IsDefault) res = "Default";
            else if (type.IsDivision) res = "Identification";
            else if (type.IsKnownStatement) res = "KnownStatement";
            else if (type.IsParagraph) res = "Paragraph";
            else if (type.IsPicture) res = "Picture";
            else if (type.IsPictureArray) res = "PictureArray";
            else if (type.IsRecord) res = "Record";
            else if (type.IsRecordArray) res = "RecordArray";
            else if (type.IsUnknownStatement) res = "UnknownStatement";
            else res = "UNRECOGNIZED TYPE!!";
            return res;
        }

        private void fillTreeView(FlowType flow, TreeNode startupNode)
        {
            string index;
            int r = 0, c = 0;
            if (flow == null) return;
            if (startupNode == null)
            {
                treeView1.Nodes.Clear();
                index = "0";
                treeView1.Nodes.Add("original" + index, "original");
                treeView1.Nodes.Add("current" + index, "current");
                treeView1.Nodes.Add("trace" + index, "trace");
            }
            else
            {
                startupNode.Nodes.Clear();
                index = startupNode.Name;
                startupNode.Nodes.Add("original" + index, "original");
                startupNode.Nodes.Add("current" + index, "current");
                startupNode.Nodes.Add("trace" + index, "trace");
            }
            
           
            TreeNode tmp;


            foreach (VariableItem item in flow.Current)
            {
                getZeroBasedRowCharPos(ref r, ref c, getRelativePositionFromZeroBasedLineAndChar(item.StartLine, item.StartChar), true);
                if (startupNode == null)
                    tmp = treeView1.Nodes["current" + index].Nodes.Add(item.StartLine + "|" + item.StartChar + "|" + item.GetHashCode(), "R: " + (r + 1) + "| C: " + (c + 1));
                else
                    tmp = startupNode.Nodes["current" + index].Nodes.Add(item.StartLine + "|" + item.StartChar + "|" + item.GetHashCode(), "R: " + (r + 1) + "| C: " + (c + 1));
                putStubsIntoTreeView(tmp);
                
            }
            foreach (VariableItem item in flow.Trace)
            {
                getZeroBasedRowCharPos(ref r, ref c, getRelativePositionFromZeroBasedLineAndChar(item.StartLine, item.StartChar), true);
                if (startupNode == null)
                    tmp = treeView1.Nodes["trace" + index].Nodes.Add(item.StartLine + "|" + item.StartChar + "|" + item.GetHashCode(), "R: " + (r + 1) + "| C: " + (c + 1));
                else
                    tmp = startupNode.Nodes["trace" + index].Nodes.Add(item.StartLine + "|" + item.StartChar + "|" + item.GetHashCode(), "R: " + (r + 1) + "| C: " + (c + 1));
                putStubsIntoTreeView(tmp);
               
            }
            if (startupNode == null)
            {
                getZeroBasedRowCharPos(ref r, ref c, getRelativePositionFromZeroBasedLineAndChar(flow.Original.StartLine, flow.Original.StartChar), true);
                treeView1.Nodes["original" + index].Text = "R: " + (r + 1) + "| C: " + (c + 1);
                treeView1.Nodes["current" + index].Expand();
                treeView1.Nodes["trace" + index].Expand();
                
            }
            else
            {
                getZeroBasedRowCharPos(ref r, ref c, getRelativePositionFromZeroBasedLineAndChar(flow.Original.StartLine, flow.Original.StartChar), true);
                startupNode.Nodes["original" + index].Text = "R: " + (r + 1) + "| C: " + (c + 1);
                startupNode.Nodes["current" + index].Expand();
                startupNode.Nodes["trace" + index].Expand();
                
            }
            
        }

        private void putStubsIntoTreeView(TreeNode node)
        {
            node.Nodes.Add("original");
            node.Nodes.Add("current");
            node.Nodes.Add("trace");
        }

        private void shiftHistory(FlowType actFlowType)
        {
            if (actFlowType == null) return;
            if (historyIndex < colors.Length - 1)
            {
                historyIndex++;
                History.Insert(historyIndex,actFlowType);
                for (int i = historyIndex + 1; i < colors.Length && i<History.Count ; i++)
                    ListReplace<FlowType>(History,i, null);
            }
            else
            {
                for (int i = 0; i < colors.Length - 1; i++)
                {
                    ListReplace<FlowType>( History,i, History[i + 1]);
                }
                ListReplace<FlowType>(History,colors.Length - 1, actFlowType);
                historyIndex = colors.Length - 1;
            }
            for (int i = colors.Length; i < History.Count; i++)
                History.RemoveAt(i);
        }

        private void ListReplace<T>(List<T> list, int indexToReplace, T element)
        {
            list.RemoveAt(indexToReplace);
            list.Insert(indexToReplace, element);
        }

        private void repaintVariablesIntoRTB()
        {
            int pos = _caretPos;
            int posCaret = getScrollPosition(txt_MainLanguage.Handle);
            RichTextBox tmp = new RichTextBox();
            tmp.Text = txt_MainLanguage.Text;
            tmp.SelectAll();
            tmp.SelectionBackColor = Color.White;
            for (int i = 0; i <= historyIndex; i++)
            {
                if (History[i] != null)
                {
                    foreach (VariableItem item in History[i].Trace)
                    {
                        tmp.SelectionStart = getRelativePositionFromZeroBasedLineAndChar(item.StartLine, item.StartChar);
                        tmp.SelectionLength = getRelativePositionFromZeroBasedLineAndChar(item.EndLine, item.EndChar) - tmp.SelectionStart;
                        tmp.SelectionBackColor = colors[colors.Length - historyIndex - 1 + i];
                    }
                    foreach (VariableItem item in History[i].Current)
                    {
                        tmp.SelectionStart = getRelativePositionFromZeroBasedLineAndChar(item.StartLine, item.StartChar);
                        tmp.SelectionLength = getRelativePositionFromZeroBasedLineAndChar(item.EndLine, item.EndChar) - tmp.SelectionStart;
                        tmp.SelectionBackColor = colors[colors.Length - historyIndex - 1 + i];
                    }
                }
            }
            
            txt_MainLanguage.Rtf = tmp.Rtf;
            txt_MainLanguage.SelectionStart = _caretPos;
            Scroll(txt_MainLanguage.Handle,posCaret);
        }

        private int CountChar(char charToCount, string stringToSearchIn)
        {
            int pos = 0, count = 0;

            while ((pos = stringToSearchIn.IndexOf(charToCount, pos)) != -1)
            {
                count++;
                pos++;
            }

            return count;
        }

        private void getZeroBasedRowCharPos(ref int rowNum, ref int charPos, int relativePosition, bool calcEscapeChar)
        {
            int esc = 0;
            int index = relativePosition;
            for (int i = 0;  i<_file.RowCount; i++)
            {
                if (index < _file.RowAt(i).SourceRow.Length + 1)
                {
                    if (calcEscapeChar)
                        esc += CountChar('\n', _file.RowAt(i).SourceRow.Substring(0,index));
                    rowNum = i;
                    charPos = index;
                    if (calcEscapeChar)
                    {
                        rowNum += esc;
                        if (_file.RowAt(i).SourceRow.Substring(0, index).LastIndexOf("\n") >= 0)
                            charPos = index - _file.RowAt(i).SourceRow.Substring(0, index).LastIndexOf("\n") - 1;
                    }
                    return;
                }
                else
                {
                    index -= _file.RowAt(i).SourceRow.Length + 1;
                    if (calcEscapeChar)
                        esc += CountChar('\n', _file.RowAt(i).SourceRow);
                }
            }

        }

        private int getRelativePositionFromZeroBasedLineAndChar(int rowNum, int charPos)
        {
            int res = 0;
            Row row;
            for (int i = 0; i < rowNum; i++)
            {
                row = _file.RowAt(i);
                if(row!=null)
                    res += row.SourceRow.Length + 1;
            }
            res += charPos;
            return res;
        }

        #region Scroll Handlers

        public class MyScrollEventArgs : EventArgs
        {
            private long m_Position;
            public MyScrollEventArgs(long position)
            {
                m_Position = position;
            }
            public long Position
            {
                get { return m_Position; }
                set { m_Position = value; }
            }
        }

        //public class MyRichTextBox : RichTextBox
        //{
        //    private bool _dontCalcPosForScroll = false;
        //    private long posForScroll = 0;

        //    public void Scroll(long pos)
        //    {
        //        posForScroll = pos;
        //        long nPos = pos;
        //        nPos <<= 16;
        //        uint wParam = (uint)ScrollBarCommands.ThumbPosition | (uint)nPos;
        //        _dontCalcPosForScroll = true;

        //        SendMessage(this.Handle, (int)Message.WmVScroll, new IntPtr(wParam), new IntPtr(0));
        //        _dontCalcPosForScroll = false;
        //    }


        //    public int getScrollPosition()
        //    {
        //        int pos;
        //        ScrollInfo scrollInfo = new ScrollInfo();
        //        scrollInfo.cbSize = (uint)Marshal.SizeOf(scrollInfo);
        //        scrollInfo.fMask = (uint)ScrollBarInfo.TrackPos;
        //        GetScrollInfo(this.Handle, (int)ScrollBarType.SbVert, ref scrollInfo);
        //        pos = scrollInfo.nTrackPos;
        //        return pos;
        //    }

        //    public event EventHandler<MyScrollEventArgs> MyVScroll;
        //    protected override void WndProc(ref System.Windows.Forms.Message m)
        //    {
        //        switch (m.Msg)
        //        {
        //            case (int)Message.WmVScroll:
        //                if (MyVScroll != null)
        //                {
        //                    if (!_dontCalcPosForScroll)
        //                    {
        //                        int pos;
        //                        ScrollInfo scrollInfo = new ScrollInfo();
        //                        scrollInfo.cbSize = (uint)Marshal.SizeOf(scrollInfo);
        //                        if ((m.WParam.ToInt32() & 0xFF) == (int)ScrollBarCommands.ThumbTrack)
        //                        {
        //                            scrollInfo.fMask = (uint)ScrollBarInfo.TrackPos;
        //                            GetScrollInfo(this.Handle, (int)ScrollBarType.SbVert, ref scrollInfo);
        //                            pos = scrollInfo.nTrackPos;
        //                        }
        //                        else
        //                        {
        //                            scrollInfo.fMask = (uint)ScrollBarInfo.Pos;
        //                            GetScrollInfo(this.Handle, (int)ScrollBarType.SbVert, ref scrollInfo);
        //                            pos = scrollInfo.nPos;
        //                        }
        //                        MyVScroll(this, new MyScrollEventArgs(pos));
        //                    }
        //                    else
        //                        MyVScroll(this, new MyScrollEventArgs(posForScroll));
                            
        //                }
        //                break;
        //        }
        //        base.WndProc(ref m);
        //    }
        //}



        private int getScrollPosition(IntPtr Handle)
        {


            int pos;
            ScrollInfo scrollInfo = new ScrollInfo();
            scrollInfo.cbSize = (uint)Marshal.SizeOf(scrollInfo);
            scrollInfo.fMask = (uint)ScrollBarInfo.All;//.Pos
            GetScrollInfo(Handle, (int)ScrollBarType.SbVert, ref scrollInfo);
            pos = scrollInfo.nPos;
            return pos;
        }


        private new void Scroll(IntPtr Handle, int pos)
        {

            long nPos = pos;
            nPos <<= 16;
            uint wParam = (uint)ScrollBarCommands.ThumbPosition | (uint)nPos;
            try
            {
                SendMessage(Handle, (int)Message.WmVScroll, new UIntPtr(wParam), new IntPtr(0));
            }
            catch (Exception) { }
        }


        //private void txt_MainLanguage_MyVScroll(object sender, MyScrollEventArgs e)
        //{
        //    try
        //    {
        //        long nPos = e.Position;
        //        nPos <<= 16;
        //        long wParam = (long)ScrollBarCommands.ThumbPosition | nPos;
        //        SendMessage(txt_DerivedLanguage.Handle, (int)Message.WmVScroll, new IntPtr(wParam), new IntPtr(0));
        //    }
        //    catch (Exception exc)
        //    {
        //        MessageBox.Show(exc.Message);
        //    }
        //}

        //private void txt_MainLanguage_VScroll(object sender, EventArgs e)
        //{
        //    try
        //    {
        //        long nPos = GetScrollPos(txt_MainLanguage.Handle, (int)ScrollBarType.SbVert);
        //        nPos <<= 16;
        //        long wParam = (long)ScrollBarCommands.ThumbPosition | nPos;
        //        SendMessage(txt_DerivedLanguage.Handle, (int)Message.WmVScroll, new IntPtr(wParam), new IntPtr(0));
        //    }
        //    catch (Exception exc)
        //    {
        //        MessageBox.Show(exc.Message);
        //    }
        //}

        //private void txt_MainLanguage_Scroll(int pos)
        //{
        //    uint wParam = (uint)ScrollBarCommands.ThumbPosition | (uint)pos;
        //    SendMessage(txt_MainLanguage.Handle, (int)Message.WmVScroll, new IntPtr(wParam), new IntPtr(0));
        //}

#endregion

        #region menu and buttons events

        private void cobolToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            cobolToolStripMenuItem_Click(sender, e);
        }

        private void SaveToolStripButton_Click(object sender, EventArgs e)
        {
            SavetoolStripMenuItem_Click(sender, e);
        }

        private void SavetoolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void TypecheckToolStripButton_Click(object sender, EventArgs e)
        {
            typecheckSourceToolStripMenuItem_Click(sender, e);
        }

        private void exportLogToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void ExportLogToolStripButton_Click(object sender, EventArgs e)
        {
            exportLogToolStripMenuItem_Click(sender, e);
        }

        private void exitToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void CloseToolStripMenuItem_Click(object sender, EventArgs e)
        {
            txt_MainLanguage.Rtf = "";
            txt_DerivedLanguage.Rtf = "";
            _file.Close();
            _file = null;
            this.Text = "";
            resetDetails();
        }

        private void resetDetails()
        {
            txt_Details1.Text = "";
            txt_Details2.Text = "";
            txt_Details3.Text = "";
            treeView1.Nodes.Clear();
        }

        private void cobolToolStripMenuItem_Click(object sender, EventArgs e)
        {
            try
            {
                OpenFileDialog1.Title = "Open File";
                OpenFileDialog1.DefaultExt = "cbl";
                OpenFileDialog1.Filter = "Cobol Files|*.cbl|Text Files|*.txt|All Files|*.*";
                OpenFileDialog1.FilterIndex = 1;
                OpenFileDialog1.FileName = string.Empty;

                if (OpenFileDialog1.ShowDialog() == DialogResult.OK)
                {

                    if (OpenFileDialog1.FileName == "")
                    {
                        return;
                    }

                    string strExt;
                    strExt = System.IO.Path.GetExtension(OpenFileDialog1.FileName);
                    strExt = strExt.ToUpper();

                    if (strExt == ".RTF")
                    {
                        txt_MainLanguage.LoadFile(OpenFileDialog1.FileName, RichTextBoxStreamType.RichText);
                    }
                    else
                    {
                        StatusLabel.Text = "Parsing file...";
                        StatusProgressBar.Style = ProgressBarStyle.Marquee;
                        Application.DoEvents();
                        _file = new CobolFile(OpenFileDialog1.FileName);
                        printText();
                        putStatusDone();
                        txt_MainLanguage.SelectionStart = 0;
                        txt_MainLanguage.SelectionLength = 0;
                    }
                    
                    this.Text = "" + OpenFileDialog1.FileName.ToString();
                }
                else
                {
                    MessageBox.Show("Open File request cancelled by user.", "Cancelled");
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message.ToString(), "Error");
                
            }
        }


        #region Typechecker Background Worker

        private void backgroundWorker1_ProgressChanged(object sender, ProgressChangedEventArgs e)
        {
            // codice per muovere la progress bar
        }

        private void backgroundWorker1_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            if (e.Error != null) MessageBox.Show(e.Error.Message);
            else txt_DerivedLanguage.Text = _file.ILText;
            putStatusDone();
        }

        private void typecheckerBackgroundWorker_DoWork(object sender, DoWorkEventArgs e)
        {
            _file.Typecheck();         
        }

        #endregion

        private void typecheckSourceToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (_file != null && typecheckerBackgroundWorker.IsBusy != true)
            {
                StatusProgressBar.Style = ProgressBarStyle.Marquee;
                StatusLabel.Text = "Typechecking...";
                typecheckerBackgroundWorker.RunWorkerAsync();
            }
        }

        #endregion

        private void txt_MainLanguage_SelectionChanged(object sender, EventArgs e)
        {
            
            int rN = 0, cN = 0;
            getZeroBasedRowCharPos(ref rN, ref cN, txt_MainLanguage.SelectionStart, true);
            toolStripStatusLabel_RC.Text = "R: " + (rN + 1) + " C: " + (cN + 1);
        }

        private void treeView1_BeforeExpand(object sender, TreeViewCancelEventArgs e)
        {
            
            string nodeKey =e.Node.Name;
            if (nodeKey.IndexOf("current") >= 0 || nodeKey.IndexOf("trace") >= 0 || nodeKey.IndexOf("original") >= 0)
            {
                return;
            }
            int r,c;
            r =Int32.Parse( nodeKey.Substring(0,nodeKey.IndexOf("|")));
            nodeKey = nodeKey.Substring(nodeKey.IndexOf("|") + 1);
            c =Int32.Parse( nodeKey.Substring(0,nodeKey.IndexOf("|")));
            Item item = _file.GetItemFromCharPos(r, c);
            FlowType flow = item.Checkout();
            if (!(flow == null))
                fillTreeView(flow, e.Node);
        }
   
        private void txt_MainLanguage_Click(object sender, EventArgs e)
        {
            _caretPos = txt_MainLanguage.SelectionStart;
        }

        private void putStatusDone()
        {
            StatusLabel.Text = "Done";
            StatusProgressBar.Value = 0;
            StatusProgressBar.Style = ProgressBarStyle.Continuous;
            Application.DoEvents();
        }

        private void treeView1_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            string nodeKey = e.Node.Name;
            int r = -1, c = -1;
            if (nodeKey.IndexOf("current") >= 0 || nodeKey.IndexOf("trace") >= 0 || nodeKey.IndexOf("original") >= 0)
            {
                return;
            }
            try
            {
                r = Int32.Parse(nodeKey.Substring(0, nodeKey.IndexOf("|")));
            }
            catch (Exception)
            { return; }
            nodeKey = nodeKey.Substring(nodeKey.IndexOf("|") + 1);
            try
            {
                c = Int32.Parse(nodeKey.Substring(0, nodeKey.IndexOf("|")));
            }
            catch (Exception)
            { return; }

            txt_MainLanguage.SelectionStart = getRelativePositionFromZeroBasedLineAndChar(r, c);
            txt_MainLanguage.ScrollToCaret();
            txt_MainLanguage_Click(null, null);
            txt_MainLanguage_DoubleClick(null, null);
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            Properties.Settings.Default.sizeX = this.Size.Width;
            Properties.Settings.Default.sizeY = this.Size.Height;
            Properties.Settings.Default.locationX = this.Location.X;
            Properties.Settings.Default.locationY = this.Location.Y;
            Properties.Settings.Default.Save();
        }

        private void b_reset_Click(object sender, EventArgs e)
        {
            resetDetails();
            paintBackColorOriginalRows(null, txt_MainLanguage);
            resetHistory();
        }
        private void resetHistory()
        {
            historyIndex = -1;
            History.Clear();
        }


    }
}
