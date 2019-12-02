c   catprep - read final tile mdex file and generate catalog & reject
c             output files
c
c vsn 1.0  B90112: initial version
c     1.1  B90116: added option to keep source_id & src columns; added
c                  option to qualify for catalog on W1&W2 both above
c                  separate S/N threshold; removed cc_flags from catalog
c                  decision
c     1.2  B90124: fixed bug in output "\Nsrc" line; added table header
c                  line identifying version and date/time of run
c     1.3  B90202: added option "-t" to intercept table header line with
c                  "\ artifact bitmasks from", replace pathname with
c                  "/Volumes/tyto1/Ab_masks_v1/unwise-<tileID>-msk.fits"
c     1.4  B90207: added IRSA-formatted output options "-ci" and "-ri";
c                  decoupled the four output options; coded final column
c                  retention; added "-v" option for 2-char version ID to
c                  be imnplanted into "source_id"; removed "-n1" option;
c                  added "glon" & "glat" to end of data rows
c     1.41 B90207: fixed bug in IRSA C/R bar placements
c     1.5  B90212: renamed elon & elat to elon_avg & elat_avg, added new
c                  elon & elat obtained from stationary ra & dec, and
c                  moved p column to the end of the row
c     1.55 B90518: include "n" in null removal from bar files
c     1.6  B90525: include "primary" flag in cat/rej separation; added
c                  "-pc" flag to include "p" column in cat output.
c     1.61 B90525: fixed possible NaN in glat and elat
c     1.7  B90906: installed SOFA-based eq2gal
c     1.8  B90909: force PMRA and PMDec to F10.5 format, clip to
c                  range -99.99999 to 99.99999
c     1.82 B91128: changed source name & collision handling; add one
c                  digit of precision to RA & Dec; tweak least
c                  significant digits to resolve name conflicts
c     1.82 B91128: changed source name & collision handling; back to
c                  previous names, but append "a" to first source with
c                  colliding name if there is a "b" or more; buffer
c                  output lines in memory until termination to do this.
c
c=======================================================================
c
                     Integer*4  MaxFld, MaxRows
                     Parameter (MaxFld = 1000, MaxRows = 250000)
c
      Character*2500 Line, OutLine, OutLines(MaxRows)
      Character*500  InFNam, OutCnam, OutRnam, OutCInam, OutRInam, NLnam
      Character*50   MaskPath
      Character*25   Field(MaxFld), FieldC(MaxFld), FieldR(MaxFld)
      Character*24   GalStr, EclStr
      Character*21   NamStr
      Character*20   names(MaxRows), nam! dynamic allocation won't work
      Character*11   Vsn, NumStr
      Character*8    CDate, CTime, tileID
      Character*4    Flag, Flag0
      Character*2    vc
      Real *8        wsnr, minw1snr, minw2snr, ra, dec, minw1w2snr,
     +               GaLong, GaLat, EcLong, EcLat, pm
      Integer*4      IArgC, LNBlnk, nHead, nArg, nArgs, IFa(MaxFld),
     +               IFb(MaxFld), NF, nSrcHdr, n, k, i, nSrc, nCout,
     +               nRout, nNamCollisn, nAppChar, wccmap, wabmap,
     +               LenHdrNsrc, IFaC(MaxFld), IFbC(MaxFld), NFC,
     +               IFaR(MaxFld), IFbR(MaxFld), NFR
      Logical*4      NeedHelp, SanityChk, GotIn, GotOut1, GotOut2, dbg,
     +               GoCat, Good1, Good2, Good12, DoMaskNam, GotOutI1,
     +               GotOutI2, CatPcol, Peq1, IzCat(MaxRows)
c
      Data Vsn/'1.82 B91128'/, nSrc/0/, nHead/0/, SanityChk/.true./,
     +     GotIn,GotOut1,GotOut2/3*.false./, nSrcHdr/-9/, dbg/.false./,
     +     nCout,nRout/2*0/, nNamCollisn/0/, minw1w2snr/5.0d0/,
     +     minw1snr,minw2snr/2*5.0d0/, LenHdrNsrc/14/, vc/'a0'/
     +     DoMaskNam/.false./, tileID/'DayNinny'/,
     +     MaskPath/'/Volumes/tyto1/Ab_masks_v1/'/,
     +     GotOutI1,GotOutI2/2*.false./, CatPcol/.false./,
     +     IzCat/MaxRows*.false./
c
      Common / VDT / CDate, CTime, Vsn
c
      namelist / catprepin / MaskPath, minw1snr, minw1w2snr, minw2snr,
     +                       vc
c
c=======================================================================
c
      nArgs = IArgc()
      NeedHelp = (nArgs .lt. 4)
1     If (NeedHelp) then
        print *,'catprep vsn ', Vsn
        print *

        print *,'Usage: catprep <flags specifications>'
        print *
        print *,'Where the following specification is REQUIRED:'
        print *,'    -i  name of a final tile mdex file'
        print *
        print *,'and at least one of the following flags and '
     +        //'specifications is REQUIRED:'
        print *,'    -c  name of the output CatWISE catalog file'
        print *,'    -r  name of the output CatWISE reject file'
        print *,'    -ci name of the output iRSA CatWISE catalog file'
        print *,'    -ri name of the output IRSA CatWISE reject file'
        print *
        print *,'The OPTIONAL flags are:'
        print *,'    -v   2-char version code ("a0")'
        print *,'    -s1  minimum w1snr (5)'
        print *,'    -s2  minimum w2snr (5)'
        print *,'    -s12 minimum snr in w1 & w2 together (5)'
        print *,'    -n   name of a catprepin namelist file'
        print *,'    -t   tile ID for use in bitmask name'
        print *,'    -pc  include "p" column at end of catalog rows (F)'
        print *,'    -d   enable debug mode'
        Print *
        stop
      end if
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      call signon('catprep')
c
      NArg = 0
2     NArg = NArg + 1
      call GetArg(NArg,Flag)
      Flag0 = Flag
      call UpCase(Flag)
c                                      ! input CatWISE file
      If (Flag .eq. '-I') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,InFNam)
        if (Access(InFNam(1:LNBlnk(InFNam)),' ') .ne. 0) then
          print *
          print *,'ERROR: file not found: ', InFNam(1:LNBlnk(InFNam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        GotIn = .true.
c                                      ! Turn debug prints on
      else if (Flag .eq. '-D') then
        dbg = .true.
        print *,'Debug prints enabled'
c                                      ! include "p" col in cat
      else if (Flag .eq. '-PC') then
        CatPcol = .true.
        if (dbg) print *,'include "p" col in cat'
c
      else if (Flag .eq. '-C') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,OutCNam)
        if (Index(OutCnam,'.tbl') .eq. 0)
     +    OutCnam = OutCnam(1:LNBlnk(OutCnam))//'.tbl'
        if (Access(OutCnam(1:LNBlnk(OutCnam)),' ') .eq. 0) then
          print *
          print *,'ERROR: Output file already exists: ', 
     +             OutCnam(1:LNBlnk(OutCnam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        GotOut1 = .true.
c
      else if (Flag .eq. '-R') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,OutRNam)
        if (Index(OutRnam,'.tbl') .eq. 0)
     +    OutRnam = OutRnam(1:LNBlnk(OutRnam))//'.tbl'
        if (Access(OutRnam(1:LNBlnk(OutRnam)),' ') .eq. 0) then
          print *
          print *,'ERROR: Output file already exists: ', 
     +             OutRnam(1:LNBlnk(OutRnam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        GotOut2 = .true.
c
      else if (Flag .eq. '-CI') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,OutCINam)
        if (Access(OutCInam(1:LNBlnk(OutCInam)),' ') .eq. 0) then
          print *
          print *,'ERROR: Output file already exists: ', 
     +             OutCInam(1:LNBlnk(OutCInam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        GotOutI1 = .true.
c
      else if (Flag .eq. '-RI') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,OutRINam)
        if (Access(OutRInam(1:LNBlnk(OutRInam)),' ') .eq. 0) then
          print *
          print *,'ERROR: Output file already exists: ', 
     +             OutRInam(1:LNBlnk(OutRInam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        GotOutI2 = .true.
c
      else if (Flag .eq. '-N') then
        call NextNarg(NArg,Nargs)
        Call GetArg(NArg,NLNam)
        if (Access(NLNam(1:LNBlnk(NLNam)),' ') .ne. 0) then
          print *
          print *,'ERROR: File not found: ', NLNam(1:LNBlnk(NLNam))
          print *
          NeedHelp = .True.
          Go to 1
        end if
        open(10, file = NLnam)
        read(10, catprepin, end = 3017, err = 3018)
        write (6, catprepin)
        close(10)
c
      else if (Flag .eq. '-V') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,vc)
        if (dbg) print *, 'version code: ', vc
c
      else if (Flag .eq. '-S1') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3007) minw1snr
        if (dbg) print *, 'minimum w1snr: ', minw1snr
c
      else if (Flag .eq. '-S2') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3008) minw2snr
        if (dbg) print *, 'minimum w2snr: ', minw2snr
c
      else if (Flag .eq. '-S12') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,NumStr)
        read (NumStr, *, err=3008) minw1w2snr
        if (dbg) print *, 'minimum w1w2snr: ', minw1w2snr
c
      else if (Flag .eq. '-T') then
        call NextNarg(NArg,Nargs)
        call GetArg(NArg,tileID)
        doMaskNam = .true.
        if (dbg) print *, 'tile ID: ', tileID
c
      Else
        print *,'ERROR: unrecognized command-line specification: '
     +          //Flag0
      end if
c 
      If (NArg .lt. NArgs) Go to 2
      if (.not.GotIn) then
        print *, 'ERROR: no input file specified ("-i")'
        NeedHelp = .True.
        Go to 1
      end if
      if (.not.(GotOut1 .or. GotOut2 .or. GotOutI1 .or. GotOutI2)) then
        print *, 'ERROR: no output files specified'
        NeedHelp = .True.
        Go to 1
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Sanity Check
5     open (10, file = InFNam)
6     read (10,'(a)', end = 3000) Line
      if (Line(1:1) .eq. '\') go to 6
      call GetFlds(Line,Field,IFa,IFb,NF)
      rewind(10)
c
      if (SanityChk) then
        if (NF .ne. 204) then
          print *,'ERROR: input is not a two-band final-tile mdex file'
          print *,'       no. of fields =', NF,'; should be 204'
          call exit(64)
        end if
c                                      ! verify some fields in mdex file
        call ChkFld(Field(3), 'ra',3)
        call ChkFld(Field(4), 'dec',4)
        call ChkFld(Field(5), 'sigra',5)
        call ChkFld(Field(6), 'sigdec',6)
        call ChkFld(Field(7), 'sigradec',7)
        call ChkFld(Field(8), 'w1x',8)
        call ChkFld(Field(9), 'w1y',9)
        call ChkFld(Field(10),'w2x',10)
        call ChkFld(Field(11),'w2y',11)
      end if
c
      if (GotOut1)  open(20, file = OutCnam)
      if (GotOut2)  open(22, file = OutRnam)
      if (GotOutI1) open(24, file = OutCInam)
      if (GotOutI2) open(26, file = OutRInam)
c                                      ! process header lines
10    read (10, '(a)', end=500) Line
      if (Line(1:1) .eq. '\') then
        if (doMaskNam) then
          if (index(Line,'artifact bitmasks from') .gt. 0) then
            Line = '\ artifact bitmasks from '
     +              //MaskPath(1:lnblnk(MaskPath))//'unwise-'
     +              //tileID//'-msk.fits'
            if (dbg) print *,'Header bitmask line: "'
     +          //Line(1:lnblnk(Line))//'"'
          end if
        end if
        if (GotOut1) write(20,'(a)') Line(1:lnblnk(Line))
        if (GotOut2) write(22,'(a)') Line(1:lnblnk(Line))
        if ((index(Line,'\Nsrc =') .gt. 0) .and. (nSrcHdr .lt. 0)) then
          LenHdrNsrc = lnblnk(Line)
          n = index(Line,'=') + 1
          read (Line(n:lnblnk(Line)), *, err = 3002) nSrcHdr
          if (nSrcHdr .gt. MaxRows) then
            print *,'ERROR: Header \nSrc line: "'
     +        //Line(1:lnblnk(Line))//'", greater than MaxRows:',
     +          MaxRows
            print *,'Increase the MaxRows parameter in catprep'
            call exit(64)
          end if
          if (dbg) print *,'Header \nSrc line: "'
     +        //Line(1:lnblnk(Line))//'"'
        end if        
        go to 10
      end if
      if (Line(1:1) .eq. '|') then
        nHead = nHead + 1
        if (nHead .eq. 1) then
          if (GotOut1) write(20,'(a)') '\ catprep vsn '//Vsn//' run on '
     +                  //CDate//' at '//CTime
          if (GotOut2) write(22,'(a)') '\ catprep vsn '//Vsn//' run on '
     +                  //CDate//' at '//CTime
          Line(IFa(8):IFb(8))     = '|   wx   '
          Line(IFa(9):IFb(9))     = '|   wy   '
          Line(IFa(171):IFb(171)) = '| elon_avg '
          Line(IFa(173):IFb(173)) = '| elat_avg '
          OutLine = '|    source_name     '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +       //'|    glon   |    glat   |    elon   |    elat   |   P |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          call GetFlds(OutLine,FieldR,IFaR,IFbR,NFR)
          OutLine = '|    source_name     '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|    glon   |    glat   |    elon   |    elat   |'
          if (CatPcol) OutLine = OutLine(1:lnblnk(OutLine))//'   P |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
          call GetFlds(OutLine,FieldC,IFaC,IFbC,NFC)
        end if
        if (nHead .eq. 2) then 
          OutLine = '|        char        '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +       //'|  double   |  double   |  double   |  double   |   i |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|        char        '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|  double   |  double   |  double   |  double   |'
          if (CatPcol) OutLine = OutLine(1:lnblnk(OutLine))//'   i |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        end if
        if (nHead .eq. 3) then 
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +       //'|    deg    |    deg    |    deg    |    deg    |   - |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|    deg    |    deg    |    deg    |    deg    |'
          if (CatPcol) OutLine = OutLine(1:lnblnk(OutLine))//'   - |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        end if
        if (nHead .eq. 4) then 
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +       //'|     --    |     --    |     --    |     --    | null|'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|     --    |     --    |     --    |     --    |'
          if (CatPcol) OutLine = OutLine(1:lnblnk(OutLine))//' null|'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        end if
        go to 10
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Generate sexagesimal name
      nSrc = nSrc + 1
      k = 3
      Read(Line(IFA(k):IFB(k)), *, err = 3001) ra
      k = 4
      Read(Line(IFA(k):IFB(k)), *, err = 3001) dec
      call SexagesNam(ra, dec, nam)
100   if (nSrc .gt. 1) then
        nAppChar = 97                  ! we could probably speed this
110     do 120 k = 1, nSrc-1           ! search up by not starting from
          If (nam .eq. names(k)) then  ! the top every time, but the pgm
            if (nAppChar .eq. 97) then ! is pretty fast, so what the heck
              nNamCollisn = nNamCollisn + 1
              OutLines(k)(21:21) = achar(nAppChar)
            end if
            nAppChar   = nAppChar    + 1
            nam(20:20) = achar(nAppChar)
            go to 110
          end if
120     continue
      end if
      names(nSrc) = nam
      NamStr = ' '//nam
c                                      ! get galactic coordinates
      call Cel2Gal(ra, dec, GaLong, GaLat)
      write(GalStr,'(2f12.6)') GaLong, GaLat
      call Cel2Ec(ra, dec, EcLong, EcLat)
      write(EclStr,'(2f12.7)') EcLong, EcLat
c                                      ! implant version code
      k = index(Line,'-')
      Line(k+3:k+9) = Line(k:k+6)
      Line(k:k+2) = '_'//vc
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Check eligibility for catalog
      k = 204
      Peq1 =  (index(Line(IFA(k):IFB(k)),'1') .ne. 0)  ! check primary code
      if (.not.Peq1) then
        GoCat = .false.
        go to 150
      end if
c
      k = 20
      if (index(Line(IFA(k):IFB(k)),'null') .eq. 0) then
        Read(Line(IFA(k):IFB(k)), *, err = 3001) wsnr
      else
        wsnr = 0.0d0
      end if
      k = 200
      if (index(Line(IFA(k):IFB(k)),'null') .eq. 0) then
        Read(Line(IFA(k):IFB(k)), *, err = 3001) wabmap
      else
        wabmap = 0
      end if
      Good1 = (wsnr .ge. minw1snr) .and. (wabmap .eq. 0)
c
      Good12 = (wsnr .ge. minw1w2snr) .and. (wabmap .eq. 0)
      k = 21
      if (index(Line(IFA(k):IFB(k)),'null') .eq. 0) then
        Read(Line(IFA(k):IFB(k)), *, err = 3001) wsnr
      else
        wsnr = 0.0d0
      end if
      k = 202
      if (index(Line(IFA(k):IFB(k)),'null') .eq. 0) then
        Read(Line(IFA(k):IFB(k)), *, err = 3001) wabmap
      else
        wabmap = 0
      end if
      Good2 = (wsnr .ge. minw2snr) .and. (wabmap .eq. 0)
c
      Good12 = Good12 .and. (wsnr .ge. minw1w2snr) .and. (wabmap .eq. 0)
c
      GoCat = Good1 .or. Good2 .or. Good12
c
150   if (GoCat) then
        IzCat(nSrc) = .true.
        OutLine = NamStr//Line(IFa(1):IFb(1))
     +         //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +         //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +         //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +         //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +         //GalStr//EclStr
        if (CatPcol)
     +    OutLine = OutLine(1:lnblnk(OutLine))//Line(IFa(204):IFb(204))
        if (index(OutLine(IFaC(126):IFbC(126)), 'null') .eq. 0) then
          read (OutLine(IFaC(126):IFbC(126)), *) pm         ! force PMRA to F10.5
          if (pm .lt. -99.99999) pm = -99.99999             ! NOTE PMRA now in col 126
          if (pm .gt.  99.99999) pm =  99.99999
          write(OutLine(IFaC(126):IFbC(126)), '(F10.5)') pm
        end if
        if (index(OutLine(IFaC(127):IFbC(127)), 'null') .eq. 0) then
          read (OutLine(IFaC(127):IFbC(127)), *) pm         ! force PMDec to F10.5
          if (pm .lt. -99.99999) pm = -99.99999             ! NOTE PMDec now in col 127
          if (pm .gt.  99.99999) pm =  99.99999
          write(OutLine(IFaC(127):IFbC(127)), '(F10.5)') pm
        end if
c       if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine)) ! JWF B91128
        OutLines(nSrc) = OutLine                                ! JWF B91128
      else
        OutLine = NamStr//Line(IFa(1):IFb(1))
     +         //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +         //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +         //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +         //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +         //GalStr//EclStr//Line(IFa(204):IFb(204))
        if (index(OutLine(IFaC(126):IFbC(126)), 'null') .eq. 0) then
          read (OutLine(IFaC(126):IFbC(126)), *) pm         ! force PMRA to F10.5
          if (pm .lt. -99.99999) pm = -99.99999             ! NOTE PMRA now in col 126
          if (pm .gt.  99.99999) pm =  99.99999
          write(OutLine(IFaC(126):IFbC(126)), '(F10.5)') pm
        end if
        if (index(OutLine(IFaC(127):IFbC(127)), 'null') .eq. 0) then
          read (OutLine(IFaC(127):IFbC(127)), *) pm         ! force PMDec to F10.5
          if (pm .lt. -99.99999) pm = -99.99999             ! NOTE PMDec now in col 127
          if (pm .gt.  99.99999) pm =  99.99999
          write(OutLine(IFaC(127):IFbC(127)), '(F10.5)') pm
        end if
c       if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine)) ! JWF B91128
        OutLines(nSrc) = OutLine                                ! JWF B91128
      end if
c
      go to 10
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                              ! update source counts in output headers
500   print *,' No. data rows processed:    ', nSrc
c
      do 1000 i = 1, nSrc
        OutLine = OutLines(i)
        if (IzCat(i)) then
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine)) 
          nCout = nCout + 1
          if (GotOutI1) then
            do 600 k = 1, NFC
              OutLine(IFaC(k):IFaC(k)) = '|'
600         continue
610         k = index(OutLine,'null')
            if (k .gt. 0) then
              OutLine(k:k+3) = '    '
              go to 610
            end if
615         Line = ''
            n = 0
            do 620 k = 1, lnblnk(OutLine)
              if (OutLine(k:k) .ne. ' ') then
                n = n + 1
                Line(n:n) = OutLine(k:k)
              end if
620         continue
            k = index(Line,'|n|')
            if (k .gt. 0) then
              Line(k+1:k+1) = ' '
              OutLine = Line
              go to 615
            end if
            write (24,'(a)') Line(1:lnblnk(Line))//'|'
          end if
        else
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          nRout = nRout + 1
          if (GotOutI2) then
            do 700 k = 1, NFR
              OutLine(IFaR(k):IFaR(k)) = '|'
700         continue
710         k = index(OutLine,'null')
            if (k .gt. 0) then
              OutLine(k:k+3) = '    '
              go to 710
            end if
715         Line = ''
            n = 0
            do 720 k = 1, lnblnk(OutLine)
              if (OutLine(k:k) .ne. ' ') then
                n = n + 1
                Line(n:n) = OutLine(k:k)
              end if
720         continue
            k = index(Line,'|n|')
            if (k .gt. 0) then
              Line(k+1:k+1) = ' '
              OutLine = Line
              go to 715
            end if
            write (26,'(a)') Line(1:lnblnk(Line))//'|'
          end if
        end if
1000  continue
c
      if (GotOut1) then
        close(20)     
        open (20, file = OutCnam, access = 'DIRECT', recl = 1,
     +        form = 'UNFORMATTED', status = 'OLD', err = 3003)
        write (NumStr,'(I6)') nCout
        do 1050 n = 1, LenHdrNsrc-7
          Write(20, rec = 7+n) ' '
1050    continue
        do 1100 n = 1, 6
          Write(20, rec = 7+n) NumStr(n:n)
1100    continue
      end if
c
1200  if (GotOut2) then
        close(22)     
        open (22, file = OutRnam, access = 'DIRECT', recl = 1,
     +        form = 'UNFORMATTED', status = 'OLD', err = 3004)
        write (NumStr,'(I6)') nRout
        do 1250 n = 1, LenHdrNsrc-7
          Write(22, rec = 7+n) ' '
1250    continue
        do 1300 n = 1, 6
          Write(22, rec = 7+n) NumStr(n:n)
1300    continue
      end if
c
1400  if (nSrc .ne. nSrcHdr) then
        print *,
     + 'ERROR: this does not match the header "\nSrc" value: ',nSrcHdr
1401    n = index(InFNam,'/')
        if (n .gt. 0) then
          do 1402 i = 1, n
          InFNam(i:i) = ' '
1402      continue
          go to 1401
        end if
        InFNam = AdjustL(InFNam)
        open (33, file = 'ERROR_MESSAGE-'
     +                  //InFNam(1:lnblnk(InFNam))//'.txt')
        write (33,'(a)') 'ERROR: source count mismatch'
        write (33,'(a,i9)') 'header "\nSrc" value:', nSrcHdr
        write (33,'(a,i9)') 'actual source count: ', nSrc
      end if
      if (GotOut1 .or. GotOutI1)  
     +    print *,' No. output catalog sources: ', nCout  
      if (GotOut2 .or. GotOutI2)
     +    print *,' No. output reject sources:  ', nRout
      if (nNamCollisn .gt. 0) print *,
     +  ' No. output name collisions: ', nNamCollisn 
c
      print *
      call signoff('catprep')
      stop
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                              ! error handling

3000  print *,'ERROR: end-of-file encountered during sanity check'
      call exit(64)
c
3001  print *,'ERROR: read error on mdex column ', k,', source #', nSrc
      call exit(64)
c
3002  print *,'ERROR: read error on "\nSrc" in header:'
      print *,'       ',Line 
      call exit(64)
c
3003  print *,'ERROR: couldn''t update output catalog file header'
     +      //' for record count'
      go to 1200
c
3004  print *,'ERROR: couldn''t update output reject file header'
     +      //' for record count'
      go to 1400
c
3007  print *,'ERROR: bad "-s1" specification: ', NumStr
      call exit(64)
c
3008  print *,'ERROR: bad "-s2" specification: ', NumStr
      call exit(64)
c
3017  print *,'ERROR: EoF encountered in catprepin namelist file'
      call exit(64)
c
3018  print *,'ERROR: read error encountered in catprepin namelist file'
      call exit(64)
c
      stop
      end
c
c=======================================================================
c
      subroutine ChkFld(Fld1, Fld2, k)
c      
      character*(*) Fld1, Fld2
      integer*4     k, lnblnk      
c
      if (Fld1 .ne. Fld2) then
        print *,'ERROR: input field no.',k,' expected to be ',
     +           Fld2(1:lnblnk(Fld2)),'; got ',Fld1(1:lnblnk(Fld2))
        call exit(64)
      end if
c      
      return
c      
      end
c      
c=======================================================================
c
      subroutine GetFlds(ColNam,Field,IFa,IFb,NF)
c-----------------------------------------------------------------------
c
c  Get fields in a table-file header line
c
c-----------------------------------------------------------------------
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      character*2500 ColNam
      Character*300  Line
      character*25   Field(MaxFld)
      integer*4      IFa(MaxFld), IFb(MaxFld), NF, N, M, L, K, LNBlnk,
     +               LastErr
c
c-----------------------------------------------------------------------
c
      N = 0
      K = 0
      LastErr = 0
      do 100 M = 1, LNBlnk(ColNam)
        if (ColNam(M:M) .eq. '|') then
          N = N + 1
          NF = N - 1
          if (N .gt. 1) IFb(N-1) = M-1
          if (N .gt. MaxFld) return
          IFa(N) = M
          do 10 L = 1, 25
            Field(N)(L:L) = ' '
10        continue
          K = 0
        else
          if (ColNam(M:M) .ne. ' ') then
            K = K + 1
            if (K .le. 25) then
              Field(N)(K:K) = ColNam(M:M)
            else
              if (LastErr .ne. N) then
                write(Line,*) N
                Line = 'GetFlds - Table column name no. '
     +               //Line(1:lnblnk(Line))//' longer than 25 '
     +               //'characters: '//Field(N)//'....; excess ignored'
                print *,Line(1:lnblnk(line))
                LastErr = N
              end if
            end if
          end if
        end if
100   continue
c
      return
      end
c      
c=======================================================================
c
      Subroutine NextNarg(NArg,NArgs)
c
      integer NArg, NArgs
c
c-----------------------------------------------------------------------
c
      if (NArg .lt. NArgs) then
        NArg = NArg + 1
        return
      else
        print *,'ERROR: expected another argument but none found'
        call exit(64)
      end if
      return
      end
c
c=======================================================================
c
      subroutine upcase(string)
      character*(*) string
      integer*4 j, lnblnk
c
      do 10 j = 1,lnblnk(string)
         if(string(j:j) .ge. "a" .and. string(j:j) .le. "z") then
            string(j:j) = achar(iachar(string(j:j)) - 32)
         end if
10    continue
      return
      end      
c      
c=======================================================================
c
      subroutine SignOn(pgmnam)
c
c *** signon- routine which provides sign-on and sign-off messages
c             (orig by John Fowler- mod by Howard McCallon-041214-SIRTF)
c
c     inputs:  pgmnam = program name                          [call arg]
c
c     outputs: message to stdout
c
      character*(*) pgmnam
      character vsn*11,cdate*8,ctime*8,Fmt*11,FLen*4
      integer*4 onoff,jdate(3),jtime(3),lnblnk
      real*4    dummyt,second(2),etime
c
      common /vdt/ cdate,ctime,vsn
c##
      onoff = 1
c
c         i. obtain date
c
100   cdate = '00-00-00'
      call idate(jdate)    ! Linux call
c
      jdate(3) = mod(jdate(3), 100)
      write(cdate(1:2), '(i2)') jdate(2)
      write(cdate(4:5), '(i2)') jdate(1)
      write(cdate(7:8), '(i2)') jdate(3)
c
      if(cdate(4:4) .eq. ' ') cdate(4:4) = '0'
      if(cdate(7:7) .eq. ' ') cdate(7:7) = '0'
c
c         ii. obtain time
c
      ctime = '00:00:00'
      call itime(jtime)
      write(ctime(1:2), '(i2)') jtime(1)
      write(ctime(4:5), '(i2)') jtime(2)
      write(ctime(7:8), '(i2)') jtime(3)
c
      if(ctime(4:4) .eq. ' ') ctime(4:4) = '0'
      if(ctime(7:7) .eq. ' ') ctime(7:7) = '0'
c
c         iii. set up format for pgmnam
c
      write(Flen,'(I4)') lnblnk(pgmnam)
      Fmt = '(A'//Flen//'$)'
c
c         iv. write out results
c
      write(*,Fmt) pgmnam
      if(onoff .eq. 1) then                      ! sign on
        write(*,301) vsn,cdate,ctime
      else                                       ! sign off
        dummyt = etime(second)
        write(*,302) vsn,cdate,ctime,second
      endif
  301 format(' version: ',a11,' - execution begun on ',a8,' at ',a8)
  302 format(' version: ',a11,' - execution ended on ',a8,' at ',a8
     *    /1x,f9.2,' cpu seconds used;',f8.2,' system seconds used.')
c
      return
c
      entry SignOff(pgmnam)
      OnOff = 2
      go to 100
c
      end
c      
c=======================================================================
c
      subroutine SexagesNam(ra, dec, nam)
c
c     Jhhmmss.ss+ddmmss.s
c     1234567890123456789
c
      character*20 nam, tmpstr
      real*8       ra, dec, tSec, aSec
      integer*4    nHH, nMM, nTmp, nDeg, nMin
c
c-----------------------------------------------------------------------
c
      nHH  = ra/15.0d0
      nMM  = 4.0d0*(ra - 15.0d0*dfloat(nHH))
      tSec = 240.0d0*(ra - 15.0d0*dfloat(nHH) - dfloat(nMM)/4.0d0)
c   
      nDeg = dabs(dec)
      nMin = 60.0d0*(dabs(dec) - dfloat(nDeg))
      aSec = 3600.0d0*(dabs(dec) - dfloat(nDeg) - dfloat(nMin)/60.0d0)
c
      write(tmpstr,'(i2.2)') nHH
      nam = 'J'//tmpstr
      write(tmpstr,'(i2.2)') nMM
      nam = nam(1:lnblnk(nam))//tmpstr
      write(tmpstr,'(f6.3)') tSec
      if (tmpstr(1:1) .eq. ' ') tmpstr(1:1) = '0'
      nam = nam(1:lnblnk(nam))//tmpstr(1:5)
c
      if (dec .ge. 0.0d0) then
        nam(11:11) = '+'
      else
        nam(11:11) = '-'
      end if
c
      write(tmpstr,'(i2.2)') nDeg
      nam = nam(1:lnblnk(nam))//tmpstr
      write(tmpstr,'(i2.2)') nMin
      nam = nam(1:lnblnk(nam))//tmpstr
      write(tmpstr,'(f6.3)') aSec
      if (tmpstr(1:1) .eq. ' ') tmpstr(1:1) = '0'
      nam = nam(1:lnblnk(nam))//tmpstr(1:4)
c
      return
      end
c
c=======================================================================
c
      subroutine Cel2Ec(RA, Dec, Long, Lat)
c
      real*8 RA, Dec, Long, Lat, SOb, Cob, X, Y, Z, d2r,
     +       cRA, cDec, sRA, sDec, X2, Y2
c
c   Obliquity in J2000: 23.43927944 deg = 8.4381406d4 arcsec  
c
      data d2r/1.745329252d-2/, cOb, sOb/0.917482143d0, 0.397776969d0/
c
c Ref: Celest Mech Dyn Astr (2011) 110:293–304
c      DOI 10.1007/s10569-011-9352-4
c      SPECIAL REPORT
c      The IAU 2009 system of astronomical constants:
c      the report of the IAU working group on numerical
c      standards for Fundamental Astronomy
c      https://link.springer.com/content/pdf/10.1007%2Fs10569-011-9352-4.pdf
c
c-----------------------------------------------------------------------
c
      cRA   = dcos(d2r*RA)
      cDec  = dcos(d2r*Dec)
      sRA   = dsin(d2r*RA)
      sDec  = dsin(d2r*Dec)
c
      X =  sDec
      Y = -cDec*sRA
      Z =  cDec*cRA
c
      X2 =  X*Cob + Y*Sob
      Y2 = -X*Sob + Y*Cob
c     Z2 =  Z
c
      if (X2 .gt.  1.0d0) X2 = 1.0d0
      if (X2 .lt. -1.0d0) X2 = -1.0d0
c
      Lat  = dasin(X2)/d2r
      Long = datan2(-Y2,Z)/d2r
      if (Long .lt. 0.0) Long = Long + 360.0
c
      return
      end
c
c=======================================================================
c
      subroutine Cel2Gal(RA, Dec, Long, Lat)
c
      real*8 RA, Dec, Long, Lat, d2r
c
      data d2r/1.745329252e-2/
c
      call iau_ICRS2G (d2r*RA, d2r*Dec, Long, Lat)
      Long = Long/d2r
      Lat  = Lat/d2r
c
      return
      end
c
c=======================================================================
c
      SUBROUTINE iau_ICRS2G ( DR, DD, DL, DB  )
*+
*  - - - - - - - - - - -
*   i a u _ I C R S 2 G
*  - - - - - - - - - - -
*
*  Transformation from ICRS to Galactic Coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DR       d      ICRS right ascension (radians)
*     DD       d      ICRS declination (radians)
*
*  Returned:
*     DL       d      galactic longitude (radians)
*     DB       d      galactic latitude (radians)
*
*  Notes:
*
*  1) The IAU 1958 system of Galactic coordinates was defined with
*     respect to the now obsolete reference system FK4 B1950.0.  When
*     interpreting the system in a modern context, several factors have
*     to be taken into account:
*
*     . The inclusion in FK4 positions of the E-terms of aberration.
*
*     . The distortion of the FK4 proper motion system by differential
*       Galactic rotation.
*
*     . The use of the B1950.0 equinox rather than the now-standard
*       J2000.0.
*
*     . The frame bias between ICRS and the J2000.0 mean place system.
*
*     The Hipparcos Catalogue (Perryman & ESA 1997) provides a rotation
*     matrix that transforms directly between ICRS and Galactic
*     coordinates with the above factors taken into account.  The
*     matrix is derived from three angles, namely the ICRS coordinates
*     of the Galactic pole and the longitude of the ascending node of
*     the galactic equator on the ICRS equator.  They are given in
*     degrees to five decimal places and for canonical purposes are
*     regarded as exact.  In the Hipparcos Catalogue the matrix elements
*     are given to 10 decimal places (about 20 microarcsec).  In the
*     present SOFA routine the matrix elements have been recomputed from
*     the canonical three angles and are given to 30 decimal places.
*
*  2) The inverse transformation is performed by the routine iau_G2ICRS.
*
*  Called:
*     iau_ANP      normalize angle into range 0 to 2pi
*     iau_ANPM     normalize angle into range +/- pi
*     iau_S2C      spherical coordinates to unit vector
*     iau_RXP      product of r-matrix and p-vector
*     iau_C2S      p-vector to spherical
*
*  Reference:
*     Perryman M.A.C. & ESA, 1997, ESA SP-1200, The Hipparcos and Tycho
*     catalogues.  Astrometric and photometric star catalogues
*     derived from the ESA Hipparcos Space Astrometry Mission.  ESA
*     Publications Division, Noordwijk, Netherlands.
*
*  This revision:   2015 January 9
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION DR, DD, DL, DB

      DOUBLE PRECISION iau_ANP, iau_ANPM

      DOUBLE PRECISION V1(3), V2(3)

*
*  L2,B2 system of galactic coordinates in the form presented in the
*  Hipparcos Catalogue.  In degrees:
*
*  P = 192.85948    right ascension of the Galactic north pole in ICRS
*  Q =  27.12825    declination of the Galactic north pole in ICRS
*  R =  32.93192    longitude of the ascending node of the Galactic
*                   plane on the ICRS equator
*
*  ICRS to galactic rotation matrix, obtained by computing
*  R_3(-R) R_1(pi/2-Q) R_3(pi/2+P) to the full precision shown:
*
      DOUBLE PRECISION R(3,3)
      DATA R(1,1), R(1,2), R(1,3),
     :     R(2,1), R(2,2), R(2,3),
     :     R(3,1), R(3,2), R(3,3) /
     :    -0.054875560416215368492398900454D0,
     :    -0.873437090234885048760383168409D0,
     :    -0.483835015548713226831774175116D0,
     :    +0.494109427875583673525222371358D0,
     :    -0.444829629960011178146614061616D0,
     :    +0.746982244497218890527388004556D0,
     :    -0.867666149019004701181616534570D0,
     :    -0.198076373431201528180486091412D0,
     :    +0.455983776175066922272100478348D0 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Spherical to Cartesian.
      CALL iau_S2C ( DR, DD, V1 )

*  ICRS to Galactic.
      CALL iau_RXP ( R, V1, V2 )

*  Cartesian to spherical.
      CALL iau_C2S ( V2, DL, DB )

*  Express in conventional ranges.
      DL = iau_ANP ( DL )
      DB = iau_ANPM ( DB )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
c
c=========================================================================
c
      DOUBLE PRECISION FUNCTION iau_ANP ( A )
*+
*  - - - - - - - -
*   i a u _ A N P
*  - - - - - - - -
*
*  Normalize angle into the range 0 <= A < 2pi.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A          d       angle (radians)
*
*  Returned:
*     iau_ANP    d       angle in range 0-2pi
*
*  This revision:  2000 December 15
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      W = MOD(A,D2PI)
      IF ( W .LT. 0D0 ) W = W + D2PI
      iau_ANP = W

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
c
c=========================================================================
c
      DOUBLE PRECISION FUNCTION iau_ANPM ( A )
*+
*  - - - - - - - - -
*   i a u _ A N P M
*  - - - - - - - - -
*
*  Normalize angle into the range -pi <= A < +pi.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A          d       angle (radians)
*
*  Returned:
*     iau_ANPM   d       angle in range +/-pi
*
*  This revision:  2000 November 25
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A

*  Pi
      DOUBLE PRECISION DPI
      PARAMETER ( DPI = 3.141592653589793238462643D0 )

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      W = MOD(A,D2PI)
      IF ( ABS(W) .GE. DPI ) W = W - SIGN(D2PI,A)
      iau_ANPM = W

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
c
c=========================================================================
c
      SUBROUTINE iau_C2S ( P, THETA, PHI )
*+
*  - - - - - - - -
*   i a u _ C 2 S
*  - - - - - - - -
*
*  P-vector to spherical coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     P        d(3)      p-vector
*
*  Returned:
*     THETA    d         longitude angle (radians)
*     PHI      d         latitude angle (radians)
*
*  Notes:
*
*  1) P can have any magnitude; only its direction is used.
*
*  2) If P is null, zero THETA and PHI are returned.
*
*  3) At either pole, zero THETA is returned.
*
*  This revision:  2007 April 11
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION P(3), THETA, PHI

      DOUBLE PRECISION X, Y, Z, D2

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      X = P(1)
      Y = P(2)
      Z = P(3)
      D2 = X*X + Y*Y

      IF ( D2 .EQ. 0D0 ) THEN
         THETA = 0D0
      ELSE
         THETA = ATAN2(Y,X)
      END IF

      IF ( Z .EQ. 0D0 ) THEN
         PHI = 0D0
      ELSE
         PHI = ATAN2(Z,SQRT(D2))
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
c
c=========================================================================
c
      SUBROUTINE iau_S2C ( THETA, PHI, C )
*+
*  - - - - - - - -
*   i a u _ S 2 C
*  - - - - - - - -
*
*  Convert spherical coordinates to Cartesian.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     THETA    d         longitude angle (radians)
*     PHI      d         latitude angle (radians)
*
*  Returned:
*     C        d(3)      direction cosines
*
*  This revision:  2000 November 25
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION THETA, PHI, C(3)

      DOUBLE PRECISION CP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      CP = COS(PHI)
      C(1) = COS(THETA) * CP
      C(2) = SIN(THETA) * CP
      C(3) = SIN(PHI)

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
c
c=========================================================================
c
      SUBROUTINE iau_RXP ( R, P, RP )
*+
*  - - - - - - - -
*   i a u _ R X P
*  - - - - - - - -
*
*  Multiply a p-vector by an r-matrix.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     R        d(3,3)    r-matrix
*     P        d(3)      p-vector
*
*  Returned:
*     RP       d(3)      R * P
*
*  Called:
*     iau_CP       copy p-vector
*
*  This revision:  2006 November 13
*
*  SOFA release 2019-07-22
*
*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R(3,3), P(3), RP(3)

      DOUBLE PRECISION W, WRP(3)

      INTEGER I, J

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Matrix R * vector P.
      DO 2 J=1,3
         W = 0D0
         DO 1 I=1,3
            W = W + R(J,I)*P(I)
 1       CONTINUE
         WRP(J) = W
 2    CONTINUE

*  Return the result.
c     CALL iau_CP ( WRP, RP )     ! JWF B90906
      RP = WRP                    ! JWF B90906

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2019
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
