namespace CobolAnalyzer.UI
{
    partial class Form1
    {
        /// <summary>
        /// Variabile di progettazione necessaria.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Liberare le risorse in uso.
        /// </summary>
        /// <param name="disposing">ha valore true se le risorse gestite devono essere eliminate, false in caso contrario.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Codice generato da Progettazione Windows Form

        /// <summary>
        /// Metodo necessario per il supporto della finestra di progettazione. Non modificare
        /// il contenuto del metodo con l'editor di codice.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.OpenToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.cobolToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.SavetoolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.SaveAsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.CloseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.toolsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.typecheckSourceToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.exportLogToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.OpenFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.OpenToolStripButton = new System.Windows.Forms.ToolStripDropDownButton();
            this.cobolToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.SaveToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.TypecheckToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.ExportLogToolStripButton = new System.Windows.Forms.ToolStripButton();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.StatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.StatusProgressBar = new System.Windows.Forms.ToolStripProgressBar();
            this.toolStripStatusLabel_RC = new System.Windows.Forms.ToolStripStatusLabel();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.txt_MainLanguage = new System.Windows.Forms.RichTextBox();
            this.txt_DerivedLanguage = new System.Windows.Forms.RichTextBox();
            this.splitContainer3 = new System.Windows.Forms.SplitContainer();
            this.panel1 = new System.Windows.Forms.Panel();
            this.txt_Details1 = new System.Windows.Forms.RichTextBox();
            this.splitContainer4 = new System.Windows.Forms.SplitContainer();
            this.panel2 = new System.Windows.Forms.Panel();
            this.b_reset = new System.Windows.Forms.Button();
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.txt_Details2 = new System.Windows.Forms.RichTextBox();
            this.txt_Details3 = new System.Windows.Forms.RichTextBox();
            this.typecheckerBackgroundWorker = new System.ComponentModel.BackgroundWorker();
            this.menuStrip1.SuspendLayout();
            this.toolStrip1.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).BeginInit();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer3)).BeginInit();
            this.splitContainer3.Panel1.SuspendLayout();
            this.splitContainer3.Panel2.SuspendLayout();
            this.splitContainer3.SuspendLayout();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer4)).BeginInit();
            this.splitContainer4.Panel1.SuspendLayout();
            this.splitContainer4.Panel2.SuspendLayout();
            this.splitContainer4.SuspendLayout();
            this.panel2.SuspendLayout();
            this.SuspendLayout();
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.OpenToolStripMenuItem,
            this.SavetoolStripMenuItem,
            this.SaveAsToolStripMenuItem,
            this.toolStripSeparator3,
            this.CloseToolStripMenuItem,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // OpenToolStripMenuItem
            // 
            this.OpenToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.cobolToolStripMenuItem});
            this.OpenToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("OpenToolStripMenuItem.Image")));
            this.OpenToolStripMenuItem.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem";
            this.OpenToolStripMenuItem.Size = new System.Drawing.Size(114, 22);
            this.OpenToolStripMenuItem.Text = "Open";
            // 
            // cobolToolStripMenuItem
            // 
            this.cobolToolStripMenuItem.Name = "cobolToolStripMenuItem";
            this.cobolToolStripMenuItem.Size = new System.Drawing.Size(106, 22);
            this.cobolToolStripMenuItem.Text = "Cobol";
            this.cobolToolStripMenuItem.Click += new System.EventHandler(this.cobolToolStripMenuItem_Click);
            // 
            // SavetoolStripMenuItem
            // 
            this.SavetoolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("SavetoolStripMenuItem.Image")));
            this.SavetoolStripMenuItem.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.SavetoolStripMenuItem.Name = "SavetoolStripMenuItem";
            this.SavetoolStripMenuItem.Size = new System.Drawing.Size(114, 22);
            this.SavetoolStripMenuItem.Text = "Save";
            this.SavetoolStripMenuItem.Click += new System.EventHandler(this.SavetoolStripMenuItem_Click);
            // 
            // SaveAsToolStripMenuItem
            // 
            this.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem";
            this.SaveAsToolStripMenuItem.Size = new System.Drawing.Size(114, 22);
            this.SaveAsToolStripMenuItem.Text = "Save As";
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(111, 6);
            // 
            // CloseToolStripMenuItem
            // 
            this.CloseToolStripMenuItem.Name = "CloseToolStripMenuItem";
            this.CloseToolStripMenuItem.Size = new System.Drawing.Size(114, 22);
            this.CloseToolStripMenuItem.Text = "Close";
            this.CloseToolStripMenuItem.Click += new System.EventHandler(this.CloseToolStripMenuItem_Click);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(114, 22);
            this.exitToolStripMenuItem.Text = "Exit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // helpToolStripMenuItem
            // 
            this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem});
            this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
            this.helpToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
            this.helpToolStripMenuItem.Text = "Help";
            // 
            // aboutToolStripMenuItem
            // 
            this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
            this.aboutToolStripMenuItem.Size = new System.Drawing.Size(107, 22);
            this.aboutToolStripMenuItem.Text = "About";
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.toolsToolStripMenuItem,
            this.helpToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(666, 24);
            this.menuStrip1.TabIndex = 0;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // toolsToolStripMenuItem
            // 
            this.toolsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.typecheckSourceToolStripMenuItem,
            this.exportLogToolStripMenuItem});
            this.toolsToolStripMenuItem.Name = "toolsToolStripMenuItem";
            this.toolsToolStripMenuItem.Size = new System.Drawing.Size(48, 20);
            this.toolsToolStripMenuItem.Text = "Tools";
            // 
            // typecheckSourceToolStripMenuItem
            // 
            this.typecheckSourceToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("typecheckSourceToolStripMenuItem.Image")));
            this.typecheckSourceToolStripMenuItem.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.typecheckSourceToolStripMenuItem.Name = "typecheckSourceToolStripMenuItem";
            this.typecheckSourceToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.typecheckSourceToolStripMenuItem.Text = "Typecheck source";
            // 
            // exportLogToolStripMenuItem
            // 
            this.exportLogToolStripMenuItem.Image = ((System.Drawing.Image)(resources.GetObject("exportLogToolStripMenuItem.Image")));
            this.exportLogToolStripMenuItem.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.exportLogToolStripMenuItem.Name = "exportLogToolStripMenuItem";
            this.exportLogToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.exportLogToolStripMenuItem.Text = "Export log";
            this.exportLogToolStripMenuItem.Click += new System.EventHandler(this.exportLogToolStripMenuItem_Click);
            // 
            // OpenFileDialog1
            // 
            this.OpenFileDialog1.FileName = "openFileDialog1";
            // 
            // toolStrip1
            // 
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.OpenToolStripButton,
            this.SaveToolStripButton,
            this.toolStripSeparator1,
            this.TypecheckToolStripButton,
            this.ExportLogToolStripButton});
            this.toolStrip1.Location = new System.Drawing.Point(0, 24);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(666, 25);
            this.toolStrip1.TabIndex = 4;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // OpenToolStripButton
            // 
            this.OpenToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.OpenToolStripButton.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.cobolToolStripMenuItem1});
            this.OpenToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("OpenToolStripButton.Image")));
            this.OpenToolStripButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.OpenToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.OpenToolStripButton.Name = "OpenToolStripButton";
            this.OpenToolStripButton.Size = new System.Drawing.Size(29, 22);
            this.OpenToolStripButton.Text = "Open";
            // 
            // cobolToolStripMenuItem1
            // 
            this.cobolToolStripMenuItem1.Name = "cobolToolStripMenuItem1";
            this.cobolToolStripMenuItem1.Size = new System.Drawing.Size(106, 22);
            this.cobolToolStripMenuItem1.Text = "Cobol";
            this.cobolToolStripMenuItem1.Click += new System.EventHandler(this.cobolToolStripMenuItem1_Click);
            // 
            // SaveToolStripButton
            // 
            this.SaveToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.SaveToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("SaveToolStripButton.Image")));
            this.SaveToolStripButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.SaveToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.SaveToolStripButton.Name = "SaveToolStripButton";
            this.SaveToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.SaveToolStripButton.Text = "Save";
            this.SaveToolStripButton.Click += new System.EventHandler(this.SaveToolStripButton_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // TypecheckToolStripButton
            // 
            this.TypecheckToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.TypecheckToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("TypecheckToolStripButton.Image")));
            this.TypecheckToolStripButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.TypecheckToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.TypecheckToolStripButton.Name = "TypecheckToolStripButton";
            this.TypecheckToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.TypecheckToolStripButton.Text = "Typecheck";
            this.TypecheckToolStripButton.Click += new System.EventHandler(this.TypecheckToolStripButton_Click);
            // 
            // ExportLogToolStripButton
            // 
            this.ExportLogToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.ExportLogToolStripButton.Image = ((System.Drawing.Image)(resources.GetObject("ExportLogToolStripButton.Image")));
            this.ExportLogToolStripButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.ExportLogToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ExportLogToolStripButton.Name = "ExportLogToolStripButton";
            this.ExportLogToolStripButton.Size = new System.Drawing.Size(23, 22);
            this.ExportLogToolStripButton.Text = "Export Log";
            this.ExportLogToolStripButton.Click += new System.EventHandler(this.ExportLogToolStripButton_Click);
            // 
            // statusStrip1
            // 
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusLabel,
            this.StatusProgressBar,
            this.toolStripStatusLabel_RC});
            this.statusStrip1.Location = new System.Drawing.Point(0, 545);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(666, 22);
            this.statusStrip1.TabIndex = 5;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // StatusLabel
            // 
            this.StatusLabel.Name = "StatusLabel";
            this.StatusLabel.Size = new System.Drawing.Size(124, 17);
            this.StatusLabel.Text = "Waiting for command";
            // 
            // StatusProgressBar
            // 
            this.StatusProgressBar.Name = "StatusProgressBar";
            this.StatusProgressBar.Size = new System.Drawing.Size(100, 16);
            this.StatusProgressBar.Step = 1;
            this.StatusProgressBar.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            // 
            // toolStripStatusLabel_RC
            // 
            this.toolStripStatusLabel_RC.Name = "toolStripStatusLabel_RC";
            this.toolStripStatusLabel_RC.Size = new System.Drawing.Size(425, 17);
            this.toolStripStatusLabel_RC.Spring = true;
            this.toolStripStatusLabel_RC.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 49);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.splitContainer2);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.splitContainer3);
            this.splitContainer1.Size = new System.Drawing.Size(666, 496);
            this.splitContainer1.SplitterDistance = 505;
            this.splitContainer1.TabIndex = 6;
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.txt_MainLanguage);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.txt_DerivedLanguage);
            this.splitContainer2.Size = new System.Drawing.Size(505, 496);
            this.splitContainer2.SplitterDistance = 252;
            this.splitContainer2.TabIndex = 0;
            // 
            // txt_MainLanguage
            // 
            this.txt_MainLanguage.BackColor = System.Drawing.Color.White;
            this.txt_MainLanguage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txt_MainLanguage.Location = new System.Drawing.Point(0, 0);
            this.txt_MainLanguage.Name = "txt_MainLanguage";
            this.txt_MainLanguage.ReadOnly = true;
            this.txt_MainLanguage.Size = new System.Drawing.Size(252, 496);
            this.txt_MainLanguage.TabIndex = 0;
            this.txt_MainLanguage.Text = "";
            this.txt_MainLanguage.WordWrap = false;
            this.txt_MainLanguage.SelectionChanged += new System.EventHandler(this.txt_MainLanguage_SelectionChanged);
            this.txt_MainLanguage.Click += new System.EventHandler(this.txt_MainLanguage_Click);
            this.txt_MainLanguage.DoubleClick += new System.EventHandler(this.txt_MainLanguage_DoubleClick);
            // 
            // txt_DerivedLanguage
            // 
            this.txt_DerivedLanguage.BackColor = System.Drawing.Color.White;
            this.txt_DerivedLanguage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txt_DerivedLanguage.Location = new System.Drawing.Point(0, 0);
            this.txt_DerivedLanguage.Name = "txt_DerivedLanguage";
            this.txt_DerivedLanguage.ReadOnly = true;
            this.txt_DerivedLanguage.Size = new System.Drawing.Size(249, 496);
            this.txt_DerivedLanguage.TabIndex = 0;
            this.txt_DerivedLanguage.Text = "";
            this.txt_DerivedLanguage.WordWrap = false;
            // 
            // splitContainer3
            // 
            this.splitContainer3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer3.Location = new System.Drawing.Point(0, 0);
            this.splitContainer3.Name = "splitContainer3";
            this.splitContainer3.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer3.Panel1
            // 
            this.splitContainer3.Panel1.Controls.Add(this.panel1);
            // 
            // splitContainer3.Panel2
            // 
            this.splitContainer3.Panel2.Controls.Add(this.splitContainer4);
            this.splitContainer3.Size = new System.Drawing.Size(157, 496);
            this.splitContainer3.SplitterDistance = 25;
            this.splitContainer3.TabIndex = 2;
            // 
            // panel1
            // 
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.txt_Details1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(157, 25);
            this.panel1.TabIndex = 1;
            // 
            // txt_Details1
            // 
            this.txt_Details1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txt_Details1.Location = new System.Drawing.Point(0, 0);
            this.txt_Details1.Name = "txt_Details1";
            this.txt_Details1.Size = new System.Drawing.Size(155, 23);
            this.txt_Details1.TabIndex = 0;
            this.txt_Details1.Text = "";
            // 
            // splitContainer4
            // 
            this.splitContainer4.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer4.Location = new System.Drawing.Point(0, 0);
            this.splitContainer4.Name = "splitContainer4";
            this.splitContainer4.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer4.Panel1
            // 
            this.splitContainer4.Panel1.Controls.Add(this.panel2);
            this.splitContainer4.Panel1MinSize = 0;
            // 
            // splitContainer4.Panel2
            // 
            this.splitContainer4.Panel2.Controls.Add(this.txt_Details3);
            this.splitContainer4.Panel2MinSize = 0;
            this.splitContainer4.Size = new System.Drawing.Size(157, 467);
            this.splitContainer4.SplitterDistance = 342;
            this.splitContainer4.TabIndex = 0;
            // 
            // panel2
            // 
            this.panel2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel2.Controls.Add(this.b_reset);
            this.panel2.Controls.Add(this.treeView1);
            this.panel2.Controls.Add(this.txt_Details2);
            this.panel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel2.Location = new System.Drawing.Point(0, 0);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(157, 342);
            this.panel2.TabIndex = 2;
            // 
            // b_reset
            // 
            this.b_reset.Location = new System.Drawing.Point(6, 42);
            this.b_reset.Name = "b_reset";
            this.b_reset.Size = new System.Drawing.Size(51, 21);
            this.b_reset.TabIndex = 2;
            this.b_reset.Text = "RESET";
            this.b_reset.UseVisualStyleBackColor = true;
            this.b_reset.Click += new System.EventHandler(this.b_reset_Click);
            // 
            // treeView1
            // 
            this.treeView1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.treeView1.Location = new System.Drawing.Point(-1, 73);
            this.treeView1.Name = "treeView1";
            this.treeView1.Size = new System.Drawing.Size(157, 268);
            this.treeView1.TabIndex = 1;
            this.treeView1.BeforeExpand += new System.Windows.Forms.TreeViewCancelEventHandler(this.treeView1_BeforeExpand);
            this.treeView1.NodeMouseDoubleClick += new System.Windows.Forms.TreeNodeMouseClickEventHandler(this.treeView1_NodeMouseDoubleClick);
            // 
            // txt_Details2
            // 
            this.txt_Details2.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.txt_Details2.Location = new System.Drawing.Point(0, 0);
            this.txt_Details2.Name = "txt_Details2";
            this.txt_Details2.Size = new System.Drawing.Size(155, 33);
            this.txt_Details2.TabIndex = 0;
            this.txt_Details2.Text = "";
            // 
            // txt_Details3
            // 
            this.txt_Details3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txt_Details3.Location = new System.Drawing.Point(0, 0);
            this.txt_Details3.Name = "txt_Details3";
            this.txt_Details3.Size = new System.Drawing.Size(157, 121);
            this.txt_Details3.TabIndex = 1;
            this.txt_Details3.Text = "";
            // 
            // typecheckerBackgroundWorker
            // 
            this.typecheckerBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(this.typecheckerBackgroundWorker_DoWork);
            this.typecheckerBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(this.backgroundWorker1_RunWorkerCompleted);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(666, 567);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.toolStrip1);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "Form1";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Cobol Analyzer";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Form1_FormClosing);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).EndInit();
            this.splitContainer2.ResumeLayout(false);
            this.splitContainer3.Panel1.ResumeLayout(false);
            this.splitContainer3.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer3)).EndInit();
            this.splitContainer3.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.splitContainer4.Panel1.ResumeLayout(false);
            this.splitContainer4.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer4)).EndInit();
            this.splitContainer4.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem OpenToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem CloseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.OpenFileDialog OpenFileDialog1;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.ToolStripButton SaveToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton TypecheckToolStripButton;
        private System.Windows.Forms.ToolStripButton ExportLogToolStripButton;
        private System.Windows.Forms.ToolStripMenuItem cobolToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem toolsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem typecheckSourceToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem exportLogToolStripMenuItem;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel StatusLabel;
        private System.Windows.Forms.ToolStripProgressBar StatusProgressBar;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.SplitContainer splitContainer2;
        private System.Windows.Forms.RichTextBox txt_MainLanguage;
        private System.Windows.Forms.RichTextBox txt_DerivedLanguage;
        private System.Windows.Forms.ToolStripMenuItem SavetoolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem SaveAsToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
        private System.Windows.Forms.ToolStripDropDownButton OpenToolStripButton;
        private System.Windows.Forms.ToolStripMenuItem cobolToolStripMenuItem1;
        private System.Windows.Forms.SplitContainer splitContainer3;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.RichTextBox txt_Details1;
        private System.Windows.Forms.SplitContainer splitContainer4;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.RichTextBox txt_Details2;
        private System.Windows.Forms.RichTextBox txt_Details3;
        private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel_RC;
        private System.Windows.Forms.TreeView treeView1;
        private System.ComponentModel.BackgroundWorker typecheckerBackgroundWorker;
        private System.Windows.Forms.Button b_reset;

    }
}

