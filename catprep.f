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
c
c=======================================================================
c
                     Integer*4  MaxFld
                     Parameter (MaxFld = 1000)
c
      Character*5000 Line, OutLine
      Character*500  InFNam, OutCnam, OutRnam, OutCInam, OutRInam, NLnam
      Character*50   MaskPath
      Character*25   Field(MaxFld), FieldC(MaxFld), FieldR(MaxFld)
      Character*24   GalStr
      Character*21   NamStr
      Character*20   names(250000), nam! dynamic allocation won't work
      Character*11   Vsn, NumStr
      Character*8    CDate, CTime, tileID
      Character*4    Flag, Flag0
      Character*2    vc
      Real *8        wsnr, minw1snr, minw2snr, ra, dec, minw1w2snr,
     +               GaLong, GaLat 
      Integer*4      IArgC, LNBlnk, nHead, nArg, nArgs, IFa(MaxFld),
     +               IFb(MaxFld), NF, nSrcHdr, n, k, i, nSrc, nCout,
     +               nRout, nNamCollisn, nAppChar, wccmap, wabmap,
     +               LenHdrNsrc, IFaC(MaxFld), IFbC(MaxFld), NFC,
     +               IFaR(MaxFld), IFbR(MaxFld), NFR
      Logical*4      NeedHelp, SanityChk, GotIn, GotOut1, GotOut2, dbg,
     +               GoCat, Good1, Good2, Good12, DoMaskNam, GotOutI1,
     +               GotOutI2
c
      Data Vsn/'1.4  B90207'/, nSrc/0/, nHead/0/, SanityChk/.true./,
     +     GotIn,GotOut1,GotOut2/3*.false./, nSrcHdr/-9/, dbg/.false./,
     +     nCout,nRout/2*0/, nNamCollisn/0/, minw1w2snr/5.0d0/,
     +     minw1snr,minw2snr/2*5.0d0/, LenHdrNsrc/14/, vc/'a0'/
     +     DoMaskNam/.false./, tileID/'DayNinny'/,
     +     MaskPath/'/Volumes/tyto1/Ab_masks_v1/'/,
     +     GotOutI1,GotOutI2/2*.false./
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
10    read (10, '(a)', end=1000) Line
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
          Line(IFa(8):IFb(8)) = '|   wx   '
          Line(IFa(9):IFb(9)) = '|   wy   '
          OutLine = '|    source_name     '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(204))
     +           //'|    glon   |    glat   |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          call GetFlds(OutLine,FieldC,IFaC,IFbC,NFC)
          OutLine = '|    source_name     '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|    glon   |    glat   |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
          call GetFlds(OutLine,FieldR,IFaR,IFbR,NFR)
        end if
        if (nHead .eq. 2) then 
          OutLine = '|        char        '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(204))
     +           //'|  double   |  double   |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|        char        '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|  double   |  double   |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        end if
        if (nHead .eq. 3) then 
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(204))
     +           //'|    deg    |    deg    |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|    deg    |    deg    |'
          if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        end if
        if (nHead .eq. 4) then 
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(204))
     +           //'|     --    |     --    |'
          if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
          OutLine = '|         --         '//Line(IFa(1):IFb(1))
     +           //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +           //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +           //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +           //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +           //'|     --    |     --    |'
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
        nAppChar = 97
110     do 120 k = 1, nSrc-1
          If (nam .eq. names(k)) then
            if (nAppChar .eq. 97) nNamCollisn = nNamCollisn + 1
            nAppChar    = nAppChar    + 1
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
c                                      ! implant version code
      k = index(Line,'-')
      Line(k+3:k+9) = Line(k:k+6)
      Line(k:k+2) = '_'//vc
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                                      ! Check eligibility for catalog
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
      if (GoCat) then
        OutLine = NamStr//Line(IFa(1):IFb(1))
     +         //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +         //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +         //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +         //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(203))
     +         //GalStr
        if (GotOut1) write(20,'(a)') OutLine(1:lnblnk(OutLine))
        nCout = nCout + 1
        if (GotOutI1) then
          do 200 k = 1, NFC
            OutLine(IFaC(k):IFaC(k)) = '|'
200       continue
210       k = index(OutLine,'null')
          if (k .gt. 0) then
            OutLine(k:k+3) = '    '
            go to 210
          end if
          Line = ''
          n = 0
          do 220 k = 1, lnblnk(OutLine)
            if (OutLine(k:k) .ne. ' ') then
              n = n + 1
              Line(n:n) = OutLine(k:k)
            end if
220       continue
          write (24,'(a)') Line(1:lnblnk(Line))//'|'
        end if
      else
        OutLine = NamStr//Line(IFa(1):IFb(1))
     +         //Line(IFa(3):IFb(9))//Line(IFa(12):IFb(34))
     +         //Line(IFa(37):IFb(38))//Line(IFa(40):IFb(43))
     +         //Line(IFa(45):IFb(48))//Line(IFa(50):IFb(121))
     +         //Line(IFa(135):IFb(186))//Line(IFa(188):IFb(204))
     +         //GalStr
        if (GotOut2) write(22,'(a)') OutLine(1:lnblnk(OutLine))
        nRout = nRout + 1
        if (GotOutI2) then
          do 300 k = 1, NFC
            OutLine(IFaR(k):IFaR(k)) = '|'
300       continue
310       k = index(OutLine,'null')
          if (k .gt. 0) then
            OutLine(k:k+3) = '    '
            go to 310
          end if
          Line = ''
          n = 0
          do 320 k = 1, lnblnk(OutLine)
            if (OutLine(k:k) .ne. ' ') then
              n = n + 1
              Line(n:n) = OutLine(k:k)
            end if
320       continue
          write (26,'(a)') Line(1:lnblnk(Line))//'|'
        end if
      end if
c
      go to 10
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c                              ! update source counts in output headers
1000  print *,' No. data rows processed:    ', nSrc
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
1001    n = index(InFNam,'/')
        if (n .gt. 0) then
          do 1002 i = 1, n
          InFNam(i:i) = ' '
1002      continue
          go to 1001
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
      character*5000 ColNam
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
      subroutine Cel2Gal(RA, Dec, Long, Lat)
c
      real*8 RA, Dec, Long, Lat, RA0, cDec0, sDec0, Long0, d2r,
     +       cRA, cDec, sRA, sDec
c
      data d2r/1.745329252d-2/, RA0/192.8595d0/, sDec0/0.4559861d0/,
     +     cDec0/0.889987d0/, Long0/122.9320d0/
c
c-----------------------------------------------------------------------
c
      cRA   = dcos(d2r*(RA-RA0))
      cDec  = dcos(d2r*Dec)
      sRA   = dsin(d2r*(RA-RA0))
      sDec  = dsin(d2r*Dec)
c      
      Lat  = dasin(sDec0*sDec + cDec0*cDec*cRA)/d2r
      Long = Long0 - datan2(cDec*sRA,cDec0*sDec - sDec0*cDec*cRA)/d2r
      
      if (Long .lt. 0.0d0) Long = Long + 360.0d0
c
      return
      end
