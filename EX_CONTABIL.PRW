#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "TBICODE.CH"

/*/{Protheus.doc} EX_CONTABIL
(      Exemplo para contabilizacao de uma rotina customizada     )
@type function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
User Function EX_CONTABIL()

Local _cTitle			:= "Contabilizacao Off-Line"
Local _bProcess			:= {|oSelf|F001(oSelf)}
Local _cDescription		:= "Este programa tem como objetivo gerar Lançamentos Contabeis Off para titulos emitidos através da importação dos arquivos CNAB"
Local _cPerg			:= Substr(FunName(),1,8)
Private _cFunction		:= Substr(FunName(),1,8)

//Cria grupo de perguntas
F006(_cPerg)

tNewProcess():New( _cFunction, _cTitle, _bProcess, _cDescription, _cPerg )

Return
/*/{Protheus.doc} F001
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F001(oSelf)

Private _nHdlPrv 		:= 0 
Private _lUsaFlag 		:= GetNewPar("MV_CTBFLAG",.F.)
Private _nTotal  		:= 0
Private _nTotDoc 		:= 0
Private _nTotProc 		:= 0
Private _nTotalB  		:= 0
Private _nTotDocB 		:= 0
Private _nTotProcB 	
Private _nTotalT  		:= 0
Private _nTotDocT 		:= 0
Private _nTotProcT		:= 0
Private _cArquivo		:= ""
Private _aFlagCTB 		:= {}
Private _aDiario		:= {}
Private _lCabecalho		:= .F.
Private _cLote	  		:= StrZero(val(LoteCont("FIN")),6)
Private _LanceiCtb 		:= .F.
Private _lSeqCorr		:= FindFunction( "UsaSeqCor" ) .And. UsaSeqCor("SE1/SE2/SE5/SEH/SEK/SEL/SET/SEU")
Private _dDataAnt		:= dDataBase
Private _cTipoEvt		:= GetNewPar( "OA_EVENTO", "")

If MV_PAR03 == 1
	_nPeriodo := MV_PAR05 - MV_PAR04 + 1 // Data Final - Data inicial
	_nPeriodo := Iif( _nPeriodo == 0, 1, _nPeriodo )
Else
	_nPeriodo := 1
Endif

oSelf:SetRegua1(_nPeriodo)

//BeginTran() 

	If MV_PAR06 == 1 .or. MV_PAR06 == 3
		F002(oSelf)
	EndIf
	
	If MV_PAR06 == 2 .or. MV_PAR06 == 3
		F007(oSelf)
		F008(oSelf)
	EndIf

	dDataBase := _dDataAnt 

//EndTran()

Return
/*/{Protheus.doc} F002
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F002(oSelf)

	Local _cSql	  	:= ''	
	Local _cEof	  	:= chr(10) + chr(13)	
	Local _dDataIni := MV_PAR04
	Local _dDataFim := MV_PAR05
	Local _cAlias   := GetNextAlias()
	Local _dBase	:= cToD('//')
	
	_cSql	:= "		SELECT						" + _cEof
	_cSql	+= "			E1.E1_FILIAL			" + _cEof
	_cSql	+= " 		,	E1.E1_CLIENTE			" + _cEof
	_cSql	+= " 		,	E1.E1_LOJA				" + _cEof
	_cSql	+= " 		,	E1.E1_PREFIXO			" + _cEof
	_cSql	+= " 		,	E1.E1_NUM				" + _cEof
	_cSql	+= " 		,	E1.E1_PARCELA			" + _cEof
	_cSql	+= " 		,	E1.E1_TIPO				" + _cEof
	_cSql	+= " 		,	E1.E1_EMISSAO			" + _cEof
	_cSql	+= " 		,	E1.R_E_C_N_O_ RECNO		" + _cEof
	_cSql	+= "		FROM						" + _cEof
	_cSql	+= RetSqlName('SE1') + " E1 			" + _cEof
	_cSql	+= "		WHERE						" + _cEof
	_cSql	+= "			E1.D_E_L_E_T_	= ''		" + _cEof
	_cSql	+= "		AND E1.E1_FILIAL	= '" + xFilial('SE1') + "'	" + _cEof	
	_cSql	+= "		AND E1.E1_EMISSAO	BETWEEN  '" + Dtos(_dDataIni)+ "' AND '" + DtoS(_dDataFim)+ "' " + _cEof
	_cSql	+= "		AND E1.E1_LA <> 'S' 			" + _cEof
	_cSql	+= "		AND E1.E1_TIPO = 'EVT'			" + _cEof
	_cSql	+= "		ORDER BY E1.E1_TIPO, E1.E1_EMISSAO, E1.R_E_C_N_O_ " + _cEof
	
	DbSelectArea('SE1')
	SE1->(DbSetOrder(2))
	
	TCQUERY _cSql New Alias (_cAlias)
	
	While !(_cAlias)->(Eof())
	
		SE1->(DbGoTo((_cAlias)->RECNO))
		
		oSelf:IncRegua1()
				
		If Len(_aDiario) > 0
			If dDataBase <> StoD((_cAlias)->E1_EMISSAO)	
				_cAliasSE1 := SE1->(GetArea())		
				F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
				RestArea(_cAliasSE1)
			EndIf
		EndIf
						
		//	Se a contabilização for pela data de emissão, altera o valor 
		//	da database e dos parametros, para efetuar a contabilização
		//	e a seleção dos registros respectivamente.				
		If MV_PAR03 == 1
			dDataBase := StoD((_cAlias)->E1_EMISSAO)	
		Endif
		
		F003(1)
			
		//EndIf
	
		(_cAlias)->(DbSkip())
	EndDo 
	
	If Len(_aDiario) > 0
		_cAliasSE1 := SE1->(GetArea())			
		F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
		RestArea(_cAliasSE1)
	EndIf 
	
	_aFlagCTB := {}
	
	(_cAlias)->(DbCloseArea())
	SE1->(DbCloseArea())

Return
/*/{Protheus.doc} F003
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F003(_nOpc)

	Local _cPadrao 	:= 	''
	Local _cSql2	:= ''	
	Local _cEof	  	:= chr(10) + chr(13)	
 	
 	If _nOpc == 1 		
		//Posiciona no cliente										  
		DbSelectArea( "SA1" )
		SA1->(DbSetOrder(1))
		SA1->(DbSeek( xFilial("SA1") + SE1->E1_CLIENTE + SE1->E1_LOJA ))
		
		//Posiciona na natureza										  
		DbSelectArea( "SED" )
		SED->(DbSetOrder(1))
		SED->(DbSeek( xFilial("SED") + SE1->E1_NATUREZ ))
		
		//Posiciona no cadastro de bancos
		DbSelectArea( "SA6" )
		SA6->(DbSetOrder(1))
		SA6->(DbSeek( xFilial("SA6" + SE1->E1_PORTADO + SE1->E1_AGEDEP + SE1->E1_NUMCON )))
		
		_cPadrao := 'Z01'
		
		_lPadrao	:=	VerPadrao(_cPadrao)
			
		//Se o lançamento padrão estiver configurado				
		If _lPadrao
			If _lUsaFlag
				aAdd(_aFlagCTB,{"E1_LA","S","SE1",SE1->(Recno()),0,0,0})
			EndIf 
				
	 		If !_lCabecalho
	 			F004(@_nHdlPrv,@_cArquivo)//Monta o cabeçalho para contabilização
			Endif
				
	  		_nTotDoc	:= DetProva(_nHdlPrv,_cPadrao,_cFunction,_cLote,,,,,,,,@_aFlagCTB,{'SE1',SE1->(recno())})
						
			_nTotal		+= _nTotDoc
			_nTotProc	+= _nTotDoc //Totaliza por processo - Caso queira começar a contabilizar o processo
			
			If _nTotDoc > 0 
				//Registro a Provisão
				_aDiario := {{"SE1",SE1->(recno()),SE1->E1_DIACTB,"E1_NODIA","E1_DIACTB"}}
			EndIf
			
		EndIf
	ElseIf _nOpc == 2	//Baixa
		
		//Posiciona no cliente										  
		DbSelectArea( "SA1" )
		SA1->(DbSetOrder(1))
		SA1->(DbSeek( xFilial("SA1") + SE5->E5_CLIENTE + SE5->E5_LOJA ))
		
		//Posiciona na natureza										  
		DbSelectArea( "SED" )
		SED->(DbSetOrder(1))
		SED->(DbSeek( xFilial("SED") + SE5->E5_NATUREZ ))
		
		//Posiciona no cadastro de bancos
		DbSelectArea( "SA6" )
		SA6->(DbSetOrder(1))
		SA6->(DbSeek( xFilial("SA6") + SE5->E5_BANCO + SE5->E5_AGENCIA + SE5->E5_CONTA ))
		
		_cPadrao 	:= 	'Z02'
		
		_lPadrao	:=	VerPadrao(_cPadrao)
		
		If _lPadrao 
	    	If _lUsaFlag
				aAdd(_aFlagCTB,{"E5_LA","S","SE5",SE5->(Recno()),0,0,0})
			EndIf 
				
	 		If !_lCabecalho
	 			F004(@_nHdlPrv,@_cArquivo)//Monta o cabeçalho para contabilização
			Endif
			
  			_nTotDoc 	:= DetProva(_nHdlPrv,_cPadrao,_cFunction,_cLote,,,,,,,,@_aFlagCTB,{'SE5',SE5->(recno())})
									
			If _nTotDoc > 0 
				//Registro a Provisão
				_aDiario := {{"SE5",SE5->(recno()),SE5->E5_DIACTB,"E5_NODIA","E5_DIACTB"}}					
			Else
				_aFlagCTB := {}	
			EndIf
		EndIf
	EndIf			
Return
/*/{Protheus.doc} F004
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F004(_nHdlPrv,_cArquivo,_lCriar)

	_lCriar		:= If(_lCriar==NIL,.F.,_lCriar)
 	_nHdlPrv	:= HeadProva(_cLote,_cFunction,Substr(cUsuario,7,6),@_cArquivo,_lCriar)
	_lCabecalho	:= .T.

Return
/*/{Protheus.doc} F005
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/ 
Static Function F005(_cArquivo,_nHdlPrv,_cLote,_nTotal,_aFlagCTB,_aDiario)

	Local _lDigita
	Local _lAglut 
	
	Default _aDiario := {}
	
	//Grava Rodap 													  
	If _nHdlPrv > 0
	
		RodaProva(_nHdlPrv,_nTotal)
	
		//Envia para Lanamento Contabil
		_lDigita := IIF(mv_par01==1,.T.,.F.)
		_lAglut  := IIF(mv_par02==1,.T.,.F.)
				
		cA100Incl(_cArquivo,_nHdlPrv,3,_cLote,_lDigita,_lAglut,,,,@_aFlagCTB,,_aDiario)
		
		_lCabecalho 	:= .F.
		_nHdlPrv 		:= 0
		
	Endif
	
	_aFlagCTB 	:= {}
	_aDiario	:= {}

Return Nil
/*/{Protheus.doc} F006
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F006(cPerg)

	aHelpPor := {}

	PutSx1(cPerg,'01','Mostra Lanc Contab. ? ','','','MV_CH1','N',1,0,0,'C','','','','','MV_PAR01','Sim','Sim','Sim','','Nao','Nao','Nao','','','','','','','','','',aHelpPor,{},{})
	PutSx1(cPerg,'02','Aglutina Lanc Contab ?','','','MV_CH2','N',1,0,0,'C','','','','','MV_PAR02','Sim','Sim','Sim','','Nao','Nao','Nao','','','','','','','','','',aHelpPor,{},{})
	PutSx1(cPerg,'03','Contabiliza Emissoes ?','','','MV_CH3','C',1,0,0,'C','','','','','MV_PAR03','Pela dt.emissao','Pela dt.emissao','Pela dt.emissao','','Pela Database','Pela Database','Pela Database','','','','','','','','','',aHelpPor,{},{} )
	PutSx1(cPerg,'04','Da Data ?             ','','','MV_CH4','D',8,0,0,'G','','','','','MV_PAR04','','','','','','','','','','','','','','','','',aHelpPor,{},{})
	PutSx1(cPerg,'05','Ate a Data ?        	','','','MV_CH5','D',8,0,0,'G','','','','','MV_PAR05','','','','','','','','','','','','','','','','',aHelpPor,{},{})
	PutSx1(cPerg,'06','Contabiliza ? 		','','','MV_CH6','N',1,0,0,'C','','','','','MV_PAR06','Provisão','Provisão','Provisão','','Baixa','Baixa','Baixa','Ambos','Ambos','Ambos','','','','','','',aHelpPor,{},{})
	
Return
/*/{Protheus.doc} F007
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F007(oSelf)

	Local _cSql	  	:= ''	
	Local _cEof	  	:= chr(10) + chr(13)	
	Local _dDataIni := MV_PAR04
	Local _dDataFim := MV_PAR05
	Local _cAlias   := GetNextAlias()
	Local _dBase	:= ''
	
	_cSql	:= "		SELECT						" + _cEof
	_cSql	+= "			E5.E5_FILIAL			" + _cEof
	_cSql	+= "		,	E5.E5_TIPODOC			" + _cEof
	_cSql	+= "		,	E5.E5_CLIFOR			" + _cEof
	_cSql	+= "		,	E5.E5_LOJA				" + _cEof
	_cSql	+= "		,	E5.E5_PREFIXO			" + _cEof
	_cSql	+= "		,	E5.E5_NUMERO			" + _cEof
	_cSql	+= "		,	E5.E5_PARCELA			" + _cEof
	_cSql	+= "		,	E5.E5_TIPO				" + _cEof
	_cSql	+= "		,	E5.E5_DATA				" + _cEof
	_cSql	+= "		,	E5.E5_SEQ				" + _cEof
	_cSql	+= "		,   E5.E5_RECPAG			" + _cEof
	_cSql	+= "		,	E5.R_E_C_N_O_ RECNO		" + _cEof
	_cSql	+= "		FROM						" + _cEof
	_cSql	+= RetSqlName('SE5') + "  E5			" + _cEof
	_cSql	+= "		WHERE						" + _cEof
	_cSql	+= "			E5.D_E_L_E_T_	= ''	" + _cEof
	_cSql	+= "		AND E5.E5_FILIAL	= '" + xFilial('SE5') + "' " + _cEof
	_cSql	+= "		AND E5.E5_DATA	BETWEEN '" + Dtos(_dDataIni)+ "' AND '" + DtoS(_dDataFim)+ "' " + _cEof
	_cSql	+= "		AND E5.E5_LA <> 'S' 							" + _cEof
	_cSql	+= "		AND E5.E5_TIPO = 'EVT'							" + _cEof
	_cSql	+= "		AND E5.E5_RECPAG = 'R'							" + _cEof
	_cSql	+= "		AND E5.E5_CLIENTE <> ''							" + _cEof
	_cSql	+= "		ORDER BY E5.E5_TIPO, E5.E5_DATA, E5.R_E_C_N_O_	" + _cEof
	
	DbSelectArea('SE5')
	SE5->(DbSetOrder(2))
	
	TCQUERY _cSql New Alias (_cAlias)
	
	While !(_cAlias)->(Eof())
	
		oSelf:IncRegua1()
		
		SE5->(DbGoTo((_cAlias)->RECNO))
			
		If Len(_aDiario) > 0
			If dDataBase <> StoD((_cAlias)->E5_DATA)
				_cAliasSE5 := SE5->(GetArea())	
				F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
				RestArea(_cAliasSE5)
			EndIf
		EndIf
						
		//	Se a contabilização for pela data de emissão, altera o valor 
		//	da database e dos parametros, para efetuar a contabilização
		//	e a seleção dos registros respectivamente.				
		If MV_PAR03 == 1
			dDataBase := StoD((_cAlias)->E5_DATA)	
		Endif
		
		F003(2)
	
		(_cAlias)->(DbSkip())
	EndDo 
	
	If Len(_aDiario) > 0
		_cAliasSE5 := SE5->(GetArea())	
		F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
		RestArea(_cAliasSE5)
	EndIf 
	
	_aFlagCTB := {}
	
	(_cAlias)->(DbCloseArea())
	SE5->(DbCloseArea())

Return
/*/{Protheus.doc} F001
@type static function
@author Ruann Carlos
@since 06/03/2017
@version 1.0
@country Brasil
@language PT-BR
@table SE1
@see ''
@obs 06/03/2017 - Criacao do fonte
/*/
Static Function F008(oSelf)

	Local _cSql	  	:= ''	
	Local _cEof	  	:= chr(10) + chr(13)	
	Local _dDataIni := MV_PAR04
	Local _dDataFim := MV_PAR05
	Local _cAlias   := GetNextAlias()
	Local _dBase	:= ''
	
	_cSql := "	SELECT													" + _cEof		
	_cSql += "		E5.R_E_C_N_O_	 RECNO, E5_DIACTB, E5_TIPO, E5_DATA	" + _cEof
	_cSql += "	FROM 													" + _cEof
	_cSql += RetSqlName('SE5') + " E5									" + _cEof
	_cSql += "	WHERE													" + _cEof
	_cSql += "				E5.E5_FILIAL  = '" + xFilial('SE5')+ "'			" + _cEof
	_cSql += "		AND 	E5.D_E_L_E_T_ = ''							" + _cEof
	_cSql += "		AND 	E5.E5_XIMPORT = 'X'							" + _cEof
	_cSql += "		AND 	E5.E5_TIPO = 'EVT'							" + _cEof
	_cSql += "		AND 	E5.E5_DATA	BETWEEN '" + Dtos(_dDataIni)+ "' AND '" + DtoS(_dDataFim)+ "' " + _cEof
	_cSql += "		AND 	E5.E5_RECPAG  = 'P'							" + _cEof
	_cSql += "		AND 	E5.E5_LA  <> 'S'							" + _cEof 
	_cSql += "		ORDER BY E5.E5_TIPO, E5.E5_DATA, E5.R_E_C_N_O_		" + _cEof
	
	DbSelectArea('SE5')
	
	TCQUERY _cSql New Alias (_cAlias)
	
	While !(_cAlias)->(Eof())
	    
		SE5->(DbGoTo((_cAlias)->RECNO))
		
		oSelf:IncRegua1()
					
		If Len(_aDiario) > 0
			If dDataBase <> StoD((_cAlias)->E5_DATA)	
				_cAliasSE5 := SE5->(GetArea())
				F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
				RestArea(_cAliasSE5)
			EndIf
		EndIf
		
		//	Se a contabilização for pela data de emissão, altera o valor 
		//	da database e dos parametros, para efetuar a contabilização
		//	e a seleção dos registros respectivamente.				
		If MV_PAR03 == 1
			dDataBase := SE5->E5_DATA
		Endif
	
		_cPadrao 	:= 	'Z03'
		
		_lPadrao	:=	VerPadrao(_cPadrao)
		
		If _lPadrao 
	    	If _lUsaFlag
				aAdd(_aFlagCTB,{"E5_LA","S","SE5",SE5->(recno()),0,0,0})
			EndIf 
				
	 		If !_lCabecalho
	 			F004(@_nHdlPrv,@_cArquivo)//Monta o cabeçalho para contabilização
			Endif
			
	  		_nTotDoc 	:= DetProva(_nHdlPrv,_cPadrao,_cFunction,_cLote,,,,,,,,@_aFlagCTB,{'SE5',SE5->(recno())})
						
			_nTotal		+= _nTotDoc
			_nTotProc	+= _nTotDoc //Totaliza por processo - Caso queira começar a contabilizar o processo
			
			If _nTotDoc > 0 
				//Registro a Provisão
				_aDiario := {{"SE5",SE5->(recno()),SE5->E5_DIACTB,"E5_NODIA","E5_DIACTB"}}					
			Else
				_aFlagCTB := {}	
			EndIf 
			
		EndIf
			
		(_cAlias)->(DbSkip())
	EndDo
	
	If Len(_aDiario) > 0
		_cAliasSE5 := SE5->(GetArea())	
		F005(_cArquivo,@_nHdlPrv,_cLote,_nTotal,@_aFlagCTB,@_aDiario)
		RestArea(_cAliasSE5)
	EndIf 
	
	_aFlagCTB := {}
	
	(_cAlias)->(DbCloseArea())
	SE5->(DbCloseArea())

Return
