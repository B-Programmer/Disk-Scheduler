VERSION 5.00
Begin VB.Form FRMSCAN 
   BackColor       =   &H8000000A&
   Caption         =   "SCAN DISK SCHEDULING ALGORITHM IMPLEMENTATION USING VB"
   ClientHeight    =   9015
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   11985
   LinkTopic       =   "Form1"
   ScaleHeight     =   9015
   ScaleWidth      =   11985
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0FF&
      Caption         =   "     Create Items   "
      BeginProperty Font 
         Name            =   "Palatino Linotype"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   5055
      Left            =   240
      TabIndex        =   8
      Top             =   120
      Width           =   10935
      Begin VB.CommandButton CmdStart 
         BackColor       =   &H00C0FFC0&
         Caption         =   "&Start"
         BeginProperty Font 
            Name            =   "Bookman Old Style"
            Size            =   12
            Charset         =   0
            Weight          =   600
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   9120
         Style           =   1  'Graphical
         TabIndex        =   59
         Top             =   4560
         Width           =   1575
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   49
         Left            =   9840
         TabIndex        =   58
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   48
         Left            =   8760
         TabIndex        =   57
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   47
         Left            =   7680
         TabIndex        =   56
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   46
         Left            =   6600
         TabIndex        =   55
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   45
         Left            =   5520
         TabIndex        =   54
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   44
         Left            =   4440
         TabIndex        =   53
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   43
         Left            =   3360
         TabIndex        =   52
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   42
         Left            =   2280
         TabIndex        =   51
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   41
         Left            =   1200
         TabIndex        =   50
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   40
         Left            =   120
         TabIndex        =   49
         Top             =   4080
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   39
         Left            =   9840
         TabIndex        =   48
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   38
         Left            =   8760
         TabIndex        =   47
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   37
         Left            =   7680
         TabIndex        =   46
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   36
         Left            =   6600
         TabIndex        =   45
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   35
         Left            =   5520
         TabIndex        =   44
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   34
         Left            =   4440
         TabIndex        =   43
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   33
         Left            =   3360
         TabIndex        =   42
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   32
         Left            =   2280
         TabIndex        =   41
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   31
         Left            =   1200
         TabIndex        =   40
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   30
         Left            =   120
         TabIndex        =   39
         Top             =   3240
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   29
         Left            =   9840
         TabIndex        =   38
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   28
         Left            =   8760
         TabIndex        =   37
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   27
         Left            =   7680
         TabIndex        =   36
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   26
         Left            =   6600
         TabIndex        =   35
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   25
         Left            =   5520
         TabIndex        =   34
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   24
         Left            =   4440
         TabIndex        =   33
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   23
         Left            =   3360
         TabIndex        =   32
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   22
         Left            =   2280
         TabIndex        =   31
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   21
         Left            =   1200
         TabIndex        =   30
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   20
         Left            =   120
         TabIndex        =   29
         Top             =   2400
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   19
         Left            =   9840
         TabIndex        =   28
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   18
         Left            =   8760
         TabIndex        =   27
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   17
         Left            =   7680
         TabIndex        =   26
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   16
         Left            =   6600
         TabIndex        =   25
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   15
         Left            =   5520
         TabIndex        =   24
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   14
         Left            =   4440
         TabIndex        =   23
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   13
         Left            =   3360
         TabIndex        =   22
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   12
         Left            =   2280
         TabIndex        =   21
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   11
         Left            =   1200
         TabIndex        =   20
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   10
         Left            =   120
         TabIndex        =   19
         Top             =   1560
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   9
         Left            =   9840
         TabIndex        =   18
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   8
         Left            =   8760
         TabIndex        =   17
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   7
         Left            =   7680
         TabIndex        =   16
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   6
         Left            =   6600
         TabIndex        =   15
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H008080FF&
         Height          =   375
         Index           =   5
         Left            =   5520
         TabIndex        =   14
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   4
         Left            =   4440
         TabIndex        =   13
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   3
         Left            =   3360
         TabIndex        =   12
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   2
         Left            =   2280
         TabIndex        =   11
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H000080FF&
         Height          =   375
         Index           =   1
         Left            =   1200
         TabIndex        =   10
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox TXTITEM 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H008080FF&
         Height          =   375
         Index           =   0
         Left            =   120
         TabIndex        =   9
         Top             =   720
         Width           =   975
      End
      Begin VB.Label Label50 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 50"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   9840
         TabIndex        =   109
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label49 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 44"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   3360
         TabIndex        =   108
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label48 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 45"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   4440
         TabIndex        =   107
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label47 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 46"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   5520
         TabIndex        =   106
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label46 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 47"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   6600
         TabIndex        =   105
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label45 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 48"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   7680
         TabIndex        =   104
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label44 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 49"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   8760
         TabIndex        =   103
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label43 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 40"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   9840
         TabIndex        =   102
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label42 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 41"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   120
         TabIndex        =   101
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label41 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 42"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   1200
         TabIndex        =   100
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label40 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 43"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   2280
         TabIndex        =   99
         Top             =   3720
         Width           =   975
      End
      Begin VB.Label Label39 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 32"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   1200
         TabIndex        =   98
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label38 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 33"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   2280
         TabIndex        =   97
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label37 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 34"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   3360
         TabIndex        =   96
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label36 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 35"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   4440
         TabIndex        =   95
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label35 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 36"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   5520
         TabIndex        =   94
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label34 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 37"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   6600
         TabIndex        =   93
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label33 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 38"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   7680
         TabIndex        =   92
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label32 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 39"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   8760
         TabIndex        =   91
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label31 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 22"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   1200
         TabIndex        =   90
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label30 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 23"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   2280
         TabIndex        =   89
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label29 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 24"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   3360
         TabIndex        =   88
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label28 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 25"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   4440
         TabIndex        =   87
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label27 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 26"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   5520
         TabIndex        =   86
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label26 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 27"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   6600
         TabIndex        =   85
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label25 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 28"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   7680
         TabIndex        =   84
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label24 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 29"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   8760
         TabIndex        =   83
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label23 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 30"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   9840
         TabIndex        =   82
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label22 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 31"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   120
         TabIndex        =   81
         Top             =   2880
         Width           =   975
      End
      Begin VB.Label Label21 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 17"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   6600
         TabIndex        =   80
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label20 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 18"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   7680
         TabIndex        =   79
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label19 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 19"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   8760
         TabIndex        =   78
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label18 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 20"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   9840
         TabIndex        =   77
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label17 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 21"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   120
         TabIndex        =   76
         Top             =   2040
         Width           =   975
      End
      Begin VB.Label Label16 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 9"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   8760
         TabIndex        =   75
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label15 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 10"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   9840
         TabIndex        =   74
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label14 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 11"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   120
         TabIndex        =   73
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label13 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 12"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   1200
         TabIndex        =   72
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label12 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 13"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   2280
         TabIndex        =   71
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label11 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 14"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   3360
         TabIndex        =   70
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label10 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 15"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   4440
         TabIndex        =   69
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label9 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 16"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   5520
         TabIndex        =   68
         Top             =   1200
         Width           =   975
      End
      Begin VB.Label Label8 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 2"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   1200
         TabIndex        =   67
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label7 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 3"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   2280
         TabIndex        =   66
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label6 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 4"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   3360
         TabIndex        =   65
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label5 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 5"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   4440
         TabIndex        =   64
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label4 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 6"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   5520
         TabIndex        =   63
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label3 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 7"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   6600
         TabIndex        =   62
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label2 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 8"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   7680
         TabIndex        =   61
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         BackColor       =   &H00008000&
         Caption         =   "  Item 1"
         BeginProperty Font 
            Name            =   "Times New Roman"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H0000FFFF&
         Height          =   375
         Left            =   120
         TabIndex        =   60
         Top             =   360
         Width           =   975
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00800000&
      Caption         =   "   SCAN DISK SCHEDULING OUTPUT WINDOW"
      BeginProperty Font 
         Name            =   "Palatino Linotype"
         Size            =   9
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0080C0FF&
      Height          =   3255
      Left            =   240
      TabIndex        =   5
      Top             =   5760
      Width           =   10095
      Begin VB.PictureBox PicScan 
         BackColor       =   &H80000012&
         BeginProperty DataFormat 
            Type            =   0
            Format          =   ""
            HaveTrueFalseNull=   0
            FirstDayOfWeek  =   0
            FirstWeekOfYear =   0
            LCID            =   1033
            SubFormatType   =   0
         EndProperty
         FillColor       =   &H00FFFFFF&
         FillStyle       =   0  'Solid
         BeginProperty Font 
            Name            =   "Palatino Linotype"
            Size            =   12
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   2895
         Left            =   120
         ScaleHeight     =   2835
         ScaleWidth      =   9795
         TabIndex        =   6
         Top             =   240
         Width           =   9855
         Begin VB.HScrollBar HScroll1 
            Height          =   270
            Left            =   0
            Max             =   32756
            TabIndex        =   7
            Top             =   2520
            Width           =   9735
         End
      End
   End
   Begin VB.TextBox TxtInitialHead 
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   375
      Left            =   3600
      TabIndex        =   4
      Top             =   5280
      Width           =   2055
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H000000FF&
      Height          =   2175
      Left            =   10440
      TabIndex        =   0
      Top             =   6720
      Width           =   1455
      Begin VB.CommandButton CmdExit 
         BackColor       =   &H00FF8080&
         Caption         =   "&Exit"
         BeginProperty Font 
            Name            =   "Courier New"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   1680
         Width           =   1215
      End
      Begin VB.CommandButton CmdReset 
         BackColor       =   &H000080FF&
         Caption         =   "&Reset"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   2
         Top             =   960
         Width           =   1215
      End
      Begin VB.CommandButton cmdScan 
         BackColor       =   &H0080FFFF&
         Caption         =   " &SCAN"
         BeginProperty Font 
            Name            =   "Microsoft Sans Serif"
            Size            =   9
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   120
         Style           =   1  'Graphical
         TabIndex        =   1
         Top             =   240
         Width           =   1215
      End
   End
   Begin VB.Label Label52 
      Alignment       =   2  'Center
      BackColor       =   &H0000FFFF&
      Caption         =   "Enter the initial disk head position:"
      BeginProperty Font 
         Name            =   "Palatino Linotype"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   240
      TabIndex        =   110
      Top             =   5280
      Width           =   3375
   End
End
Attribute VB_Name = "FRMSCAN"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Declare Function GetTickCount Lib "kernel32" () As Long
Const Maxs As Integer = 50, Max As Integer = 199
Dim n As Integer
Dim Data(-1 To Maxs) As Long, h As Long, T(-1 To Maxs + 1) As Long
Dim create As Boolean
Const INCH = 1440
Dim lngTime As Long

Private Sub cmdScan_Click()
On Error GoTo LSearchEr
Dim i As Integer, Count As Long, ast As Double
If (create = True) Then
 If (TxtInitialHead.Text <> "") Then
 h = TxtInitialHead.Text
 Call SCAN(n, h, Count, ast) 'call to procedure to perform SCAN Disk Scheduling
 PicScan.Cls
 PicScan.Print "  This new (SCAN) list of all items entered "
 PicScan.Print
  If n > 30 Then
   For i = 0 To 30
   PicScan.Print T(i); Spc(2);
    Next i
    PicScan.Print
    For i = 31 To n
   PicScan.Print T(i); Spc(2);
    Next i
    Else
    For i = 0 To n
   PicScan.Print T(i); Spc(2);
    Next i
    End If
  PicScan.Print: PicScan.Print
  PicScan.Print "The Total Head Movement is: "; Count
  PicScan.Print " The Average Seek Time is:  "; FormatNumber(ast, 2)
  PicScan.Print "The Execution time is: "; CStr((GetTickCount - lngTime) / 1000); "secs."
  
 Else
 MsgBox " Make Sure you enter the initial disk head position in the available textbox", , "Enter Search"
 TxtInitialHead.SetFocus
 End If
Else
MsgBox "Ensure that you create Elements you wish to test", , "Create elements"
CmdStart.SetFocus
End If
Exit Sub
LSearchEr:
MsgBox "Application terminated! error has been encountered " & vbCrLf & vbCrLf & Err.Description & Err.Number, vbCritical, "Error report"
On Error GoTo 0
End Sub

Private Sub CmdExit_Click()
Beep
Unload Me
End Sub
Private Sub CmdReset_Click()
Dim i As Integer
For i = 0 To Maxs - 1
'Data(i) = ""
TXTITEM(i).Text = ""
Next i
create = False
PicScan.Cls
TxtInitialHead.Text = ""
CmdStart.Enabled = True
CmdStart.SetFocus
End Sub
Private Sub CmdStart_Click()
On Error GoTo starter
Dim i As Integer
lngTime = GetTickCount 'start timing now
n = InputBox("Enter the total number of elements to create between 1 and 50", "Create Elements")
If IsNumeric(n) Then
 If (n <= 0) Then
 MsgBox "Number of elements cannot be less or equal to zero", , "Wrong Value"
 create = False: Exit Sub
 Else
 If (n <= Maxs) Then
 MsgBox "Read in all the cylinder numbers or track location to be created "
  For i = 0 To n - 1
  Data(i) = Val(InputBox("Enter Item to create: " & i + 1, "Enter"))
  TXTITEM(i) = Data(i)
  Next i
  create = True: CmdStart.Enabled = False: TxtInitialHead.SetFocus: Exit Sub
  Else
  MsgBox "Number of Data to be created is more than available space; Make sure Number is less than or equal to 50 ", , "OverfloW"
  create = False: Exit Sub
  End If
  End If
  Else
  MsgBox "Ensure that your input is Numeric value! ", , "Wrong Input"
  create = False: Exit Sub
  End If
  Exit Sub
starter:
MsgBox "Application terminated! Error encountered " & vbCrLf & vbCrLf & Err.Description, vbCritical, "Error"
On Error GoTo 0
End Sub

Private Sub SCAN(ByVal n As Integer, ByVal h As Long, ByRef cnt As Long, ast As Double)
  Dim i As Integer, k As Integer, j As Integer, L   As Integer, R As Integer, Temp As Integer
  Dim LE(0 To 50) As Integer, RE(0 To 50) As Integer
  j = 0: L = 0: R = 0
     For i = 0 To n - 1
       If (Data(i) > h) Then
            RE(R) = Data(i): R = R + 1
       Else
            LE(L) = Data(i): L = L + 1
       End If
     Next i
     Call sort(LE, L)
     Call sort(RE, R)
     Temp = h
     For i = L - 1 To 0 Step -1
           T(j) = LE(i)
           cnt = cnt + Abs(Temp - T(j))
           'st[J] = *cnt;
           Temp = T(j):   j = j + 1
     Next i
     T(j) = 0
     cnt = cnt + Abs(LE(0) - T(j))
      'st[J] = *cnt;
      Temp = T(j): j = j + 1
     For i = 0 To R - 1
           T(j) = RE(i)
           cnt = cnt + Abs(Temp - T(j))
           'st[J] = *cnt;
           Temp = T(j):   j = j + 1
     Next i
     ast = cnt / n
End Sub

Private Sub sort(a() As Integer, n As Integer)
     Dim i As Integer, j As Integer, tmp As Integer
     For i = 0 To n - 1
          For j = i + 1 To n - 1
              If (a(i) > a(j)) Then
              tmp = a(i): a(i) = a(j): a(j) = tmp
              End If
          Next j
     Next i
End Sub

Private Sub Form_Load()
Dim i As Integer
For i = 0 To Maxs - 1
TXTITEM(i).Locked = True
Next i
 create = False
CmdStart.ToolTipText = "Click Me to start Creating elements in the boxes"
cmdScan.ToolTipText = "Make Sure you click start before you click me "
CmdStart.Enabled = True
CmdStart.TabIndex = 0
HScroll1.Max = (10 * INCH - (Me.Width * 0.5))
HScroll1.Min = -0.15 * INCH
HScroll1.LargeChange = INCH
HScroll1.SmallChange = INCH / 2
PicScan.Width = 20 * INCH
End Sub

Private Sub HScroll1_Change()
HScroll1_Scroll
End Sub

Private Sub HScroll1_Scroll()

PicScan.Left = HScroll1.Value * -1
PicScan.Width = PicScan.Width + HScroll1.Value
Call cmdScan_Click
HScroll1.Width = HScroll1.Width + 1
End Sub

Private Sub TxtInitialHead_LostFocus()
cmdScan.SetFocus
End Sub


