#====================================================================================================================
# TABLEAU DE BORD SODECOTON - CAMEROUN V01 -2020. Auteur : Samuel Talle
#===================================================================================================================


#=========================================================================================================================
#--chargemement des librairies--------------------------------------------
#=========================================================================================================================
library(shiny)
library(shinydashboard)
library(RMariaDB)
library("DBI")
library(RODBC)
library(lattice)
library(ggplot2)
library(tidyverse)
library(dplyr)
require(gridExtra)

library(DT)
library(plotly)
library(waterfalls)
library(RODBCext)
library(pivottabler)
library(rpivotTable)
theme_set(theme_bw())
library(shinyjs)
library(scales)
library(lubridate)
library(rAmCharts4)
library(rAmCharts)
#library(shinydashboardPlus)
#library(shinyscreenshot)

#=========================================================================================================================
#---Connection  la BD--------------------------------------------------------------------  
#=========================================================================================================================
#cn <- odbcDriverConnect(connection="Driver={SQL Server};server=10.100.2.26;Port=1433;database=DWH_Coton;uid=SA;pwd=Sdcc@2021;")
cn <- odbcDriverConnect(connection="Driver={SQL Server};server=DI-LPTP-16;Port=1433;database=DWH_Coton;uid=SA;pwd=Damico5089@;")

#------------------------------------------------------------------------------------------

#=========================================================================================================================
#---Verification des MAJ de la base de donnees----------------------------------------
#=========================================================================================================================
check_for_update <- function() {
    sqlQuery(cn, "SELECT MAX(timestamp) FROM  [FactProduction]")
}
#--------------------------------------------------------------------------------

#=========================================================================================================================
#---Obtention des jeux de donnees--------------------------------------------------------------
#=========================================================================================================================

get_dataProjet <- function() {
  sqlQuery(cn,  
    "
                select d.Year as Periode, d.DayOfWeek, d.MonthName, 
                    d.Date DateOpe, d.Month, realName, fisrtName, pr.name Projet, niveauAvancement NiveauAvancement,
					          respectDelai RespectDelai
                from FaitProjet p
                left join DimDate d on d.IdDateSK=p.IdDateSK
                left join DimTechnicien t on t.IdTechnicienSK=p.IdTechnicienSK
                left join DimProjet pr on pr.idProjetSK=p.idProjetSK
                where RealEndDate is null
                
          "  
  )
}

get_dataHuilerie <- function() {
  sqlQuery(cn, 
           "
                select 
                    d.Year as Periode, d.DayOfWeek, d.MonthName, 
                    d.Date DateOpe, d.Month, h.CodeUsine, h.NomUsine Usine, q.ChefQuart, t.Libelle Equipe, q.NomQuart Quart, 
                    p.GraineRecepKG, p.EcartKG, p.GraineMoKG, p.AlibetProduitSac, p.Farine21ProduitSac,
                    CoqueIncorporeKG, CoqueChaudiereKG, HuileNProduitLT, HuileNMOLT, HuileRaffineLT, NbCartonDiamaorU,
      					-- on calcule tous les kpis
      					case 
      					    when GraineMoKG = 0 then 0.0 else (HuileNProduitLT / GraineMoKG) * 100 end as TauxHnGr, 
      					case
      					    when GraineMoKG = 0 then 0.0 else ((p.AlibetProduitSac + p.Farine21ProduitSac) / GraineMoKG) * 100 end as TauxTtxGr
      
                      from FaitHlProduction p
                      left join DimDate d on d.IdDateSK=p.IdDateSK
                      left join DimQuart q on q.IdQuartSK=p.IdQuartSK
                      left join DimTranche_horaire t on t.IdTrancheSK=p.IdTrancheSK
                      left join DimUsinehuilerie h on h.IdUsineSK=p.IdUsineSK
                
          "  
  )
}


get_dataProduction <- function() {
    sqlQuery(cn, 
             "
             select 
                b.Year as Periode,b.DayOfWeek,b.MonthName,
                b.Date DateOpe, b.Month, e.IdUsine, e.Libelle Usine, c.ChefDeQuart,c.Libelle Equipe,d.Libelle Quart, 
                a.NbBalles,a.PoidsBrut,a.PoidsNet,a.PoidsBrut-a.PoidsNet Ecart_Poids,
                KwhReseau,TempsEgreneuse1,TempsEgreneuse2,TempsEgreneuse3,TempsEgreneuse4,a.NbEgreneuses,a.NbScies,
                a.TempsCondenseur
                from FactProduction a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimQuartEgrenage c on c.IdQuartEgrenageSK=a.IdQuartEgrenageSK
                left join DimQuartPeriode d on d.IdQuartPeriode=a.IdQuartPeriodeSK
                left join DimUsine e on e.IdUsineSK=a.IdUsineSK
                
          "  
    )
}

get_dataProductionUsine <- function() {
    sqlQuery(cn, 
             "
             select 
                b.Year as Periode,b.DayOfWeek,b.MonthName,
                b.Date DateOpe, b.Month, e.IdUsine, e.Libelle Usine,
                a.PdsCgEgrene,a.PdsGraineHuileries,a.PdsSemences,a.PdsGraines,a.NbSacGraines,
                a.PdsFibreCourtes,NbSacFibreCourtes,a.TempEgreneuse,a.NbScies
                from FactProductionJour a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimUsine e on e.IdUsineSK=a.IdUsineSK
                
          "  
    )
}


get_dataEvacuation <- function() {
    sqlQuery(cn, 
             "
           select 
            a.IdUsine,DateBordereauLivraison,DateProduction,
            b.Year as Periode,b.DayOfWeek,b.MonthName,
            b.Date DateOpe, b.Month, e.IdUsine, e.Libelle Usine,
            Transporteur,Chauffeur,
            1 NbBalles,PoidsBrut,a.PoidsNet,
            DATEDIFF(DAY,DateProduction,DateBordereauLivraison) delai
            from T_Evacuation a
            left join DimDate b on b.Date=a.DateBordereauLivraison
            left join DimUsine e on e.IdUsineSK=a.IdUsine
         "  
    )
}

'
get_dataClassement <- function() {
  sqlQuery(cn, 
           "
             select top 2000
              b.Year as Periode,b.DayOfWeek,b.MonthName,b.Date DateOpe, b.Month,
              c.IdVarietes,c.Libelle Variete,
              d.IdClasse,d.Libelle Classe,
              e.IdNatureCotons,e.Libelle NatureCoton,
              f.IdUsine,f.Libelle Usine,
              g.IdSoies,g.Libelle Soie, g.Longueur LongueurSoie,
              a.PoidsBrut,a.PoidsNet
              from FactClassement a
              left join DimDate b on b.IdDateSK=a.IdDateSK
              left join DimVarietes c on c.IdVarietesSK=a.IdVarietesSK
              left join DimClasse d on d.IdClasseSK=a.IdClasseSK
              left join DimNatureCotons e on e.IdNatureCotonsSK=a.IdNatureCotonsSK
              left join DimUsine f on f.IdUsineSK=a.IdUsineSK
              left join DimSoies g on g.IdSoiesSK=a.IdSoiesSK
              where b.Year=2018
          "  
  )
}
'
VYear<-2020
VIdUsune<-"0"
VIdHuilerie<-"1"
VMonth<-1
VMonthPrec<-0
nb<-0
value1G<-0

VCodeSociete<<-'Sodecoton'

get_dataIncident <- function() {
  req<- 
    "
                select 
                    d.Year as Periode, d.DayOfWeek, d.MonthName, 
                    d.Date DateOpe, d.Month, realName, fisrtName, c.name Categorie, s.name Site, di.name Direction, idTickets Tickets
      
                      from FaitIncident i
                      left join DimDate d on d.IdDateSK=i.IdDateSK
                      left join DimTechnicien t on t.IdTechnicienSK=i.IdTechnicienSK
                      left join DimCategorieInfo c on c.IdCategorieSK=i.IdCategorieSK
                      left join DimSite s on s.idSiteSk=i.idSiteSk
                      left join DimStatut st on st.idStatutSK = i.idStatutSK
                      left join DimDirection di on di.idDirectionSK = i.idDirectionSK
                      where d.Year= cast(? as varchar) and d.Month=cast(? as varchar) 
          "  
  myvalue<-data.frame(VYear, VMonth)
  sqlExecute(cn, req, myvalue, fetch=TRUE)
}

get_dataProjetParam <- function() {
  req<- 
    "
                select d.Year as Periode, d.DayOfWeek, d.MonthName, 
                    d.Date DateOpe, d.Month, realName, fisrtName, pr.name Projet, niveauAvancement NiveauAvancement,
					          respectDelai RespectDelai
                from FaitProjet p
                left join DimDate d on d.IdDateSK=p.IdDateSK
                left join DimTechnicien t on t.IdTechnicienSK=p.IdTechnicienSK
                left join DimProjet pr on pr.idProjetSK=p.idProjetSK   
                where d.Year= cast(? as varchar) and d.Month=cast(? as varchar)     
                
          "  
  myvalue<-data.frame(VYear, VMonth)
  sqlExecute(cn, req, myvalue, fetch=TRUE)
}

get_dataConsommable <- function() {
  req<- 
    "
                select d.Year as Periode, d.DayOfWeek, d.MonthName, d.Date DateOpe, d.Month, s.name Site, di.name Direction, Nombre
                 from FaitConsommableInfo c
                 left join DimDate d on d.IdDateSK=c.IdDateSK
                 left join DimSite s on s.idSiteSk=c.idSiteSk
                 left join DimDirection di on di.idDirectionSK=c.idDirectionSK                 
                 where d.Year= cast(? as varchar) and d.Month=cast(? as varchar)    
                
          "  
  myvalue<-data.frame(VYear, VMonth)
  sqlExecute(cn, req, myvalue, fetch=TRUE)
}

get_dataSatisfaction <- function() {
  req<-
    "
                select d.Year as Periode, d.DayOfWeek, d.MonthName, 
                    d.Date DateOpe, d.Month, realName, fisrtName, s.name Site, c.Name, Poids ,
					          Satisfaction, 
							  case when(satisfaction <> -1) then satisfaction else 0 end Votant,
							  case when(satisfaction = -1) then 1 else 0 end NonVotant
                from FaitSatisfaction sa
                left join DimDate d on d.IdDateSK=sa.IdDateSK
                left join DimTechnicien t on t.IdTechnicienSK=sa.IdTechnicienSK
                left join DimSite s on s.idSiteSk=sa.idSiteSk 
				        left join DimCategorieInfo c on c.idCategorieSK = sa.idCategorieSK
                where d.Year= cast(? as varchar) and d.Month=cast(? as varchar) 
          "  
  myvalue<-data.frame(VYear, VMonth)
  sqlExecute(cn, req, myvalue, fetch=TRUE)
}


get_dataHuilerie_param <- function() {
  req<-"
                select 
                    d.Year as Periode, d.DayOfWeek, d.MonthName, day(d.date) Jour,
                    d.Date DateOpe, d.Month, h.CodeUsine, h.NomUsine Usine, q.ChefQuart, t.Libelle Equipe, q.NomQuart Quart, 
                    p.GraineRecepKG, p.EcartKG, p.GraineMoKG, p.AlibetProduitSac, p.Farine21ProduitSac,  p.AlibetProduitSac + p.Farine21ProduitSac as Tourteau,
                    CoqueIncorporeKG, CoqueChaudiereKG, HuileNProduitLT, HuileNMOLT, HuileRaffineLT, NbCartonDiamaorU,
      					-- on calcule tous les kpis
      					case 
      					    when GraineMoKG = 0 then 0.0 else (HuileNProduitLT / GraineMoKG) * 100 end as TauxHnGr, 
      					case
      					    when GraineMoKG = 0 then 0.0 else ((p.AlibetProduitSac + p.Farine21ProduitSac) / GraineMoKG) * 100 end as TauxTtxGr
      
                      from FaitHlProduction p
                      left join DimDate d on d.IdDateSK=p.IdDateSK
                      left join DimQuart q on q.IdQuartSK=p.IdQuartSK
                      left join DimTranche_horaire t on t.IdTrancheSK=p.IdTrancheSK
                      left join DimUsinehuilerie h on h.IdUsineSK=p.IdUsineSK
                      where d.Year= cast(? as varchar) and d.Month=cast(? as varchar) and h.CodeUsine = cast(? as varchar)
                      order by q.NomQuart
                
          "  
  myvalue<-data.frame(VYear, VMonth, VIdHuilerie)
  sqlExecute(cn, req, myvalue, fetch=TRUE)
}


get_dataClassement <- function() { #Obtention des donnees 
  req<-"select 
              b.Year as Periode,b.DayOfWeek,b.MonthName,b.Date DateOpe, b.Month,
              c.IdVarietes,c.Libelle Variete,
              d.IdClasse,d.Libelle Classe,d.Qualite,
              e.IdNatureCotons,e.Libelle NatureCoton,
              f.IdUsine,f.Libelle Usine,
              g.IdSoies,g.Libelle Soie, g.Longueur LongueurSoie,
              h.IdAvaries,h.Libelle Avaries,
              a.PoidsBrut,a.PoidsNet
              from FactClassement a
              left join DimDate b on b.IdDateSK=a.IdDateSK
              left join DimVarietes c on c.IdVarietesSK=a.IdVarietesSK
              left join DimClasse d on d.IdClasseSK=a.IdClasseSK
              left join DimNatureCotons e on e.IdNatureCotonsSK=a.IdNatureCotonsSK
              left join DimUsine f on f.IdUsineSK=a.IdUsineSK
              left join DimSoies g on g.IdSoiesSK=a.IdSoiesSK
              left join DimAvaries h on h.IdAvariesSK=a.IdAvariesSK
              where b.Year= cast(? as varchar) and b.Month=cast(? as varchar) and f.IdUsine = cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth,VIdUsine)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
}

get_dataPanne <- function() { #Obtention des donnees 
  req<-"select 
              b.Year as Periode,b.DayOfWeek,b.MonthName,b.Date DateOpe, b.Month,
			  c.IdUsine,c.Libelle Usine,
			  d.Id_type,d.Libelle_type TypePanne,
              a.NbPanne,
              case 
               when a.DureePanne<0 then -a.DureePanne
               else a.DureePanne
              end as  DureePanne,
              clasification_machi
              from FactPanne a
              left join DimDate b on b.IdDateSK=a.IdDateSK
              left join DimUsine c on c.IdUsineSK=a.IdUsineSK
              left join DimTypePanne d on d.IdTypePanneSK=a.IdTypePanneSK      
              where b.Year= cast(? as varchar) and b.Month=cast(? as varchar) and c.IdUsine = cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth,VIdUsine)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
}

get_dataPanneCumul <- function() { #Obtention des donnees 
  req<-"select 
              b.Year as Periode,b.DayOfWeek,b.MonthName,b.Date DateOpe, b.Month,
			  c.IdUsine,c.Libelle Usine,
			  d.Id_type,d.Libelle_type TypePanne,
              a.NbPanne,
              case 
               when a.DureePanne<0 then -a.DureePanne
               else a.DureePanne
              end as  DureePanne,
              clasification_machi
              from FactPanne a
              left join DimDate b on b.IdDateSK=a.IdDateSK
              left join DimUsine c on c.IdUsineSK=a.IdUsineSK
              left join DimTypePanne d on d.IdTypePanneSK=a.IdTypePanneSK      
              where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar) and c.IdUsine = cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth,VIdUsine)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
}

get_dataPanneCumul12Mois <- function() { #Obtention des donnees 
  req<-"select 
              b.Year as Periode,b.DayOfWeek,b.MonthName,b.Date DateOpe, b.Month,
			  c.IdUsine,c.Libelle Usine,
			  d.Id_type,d.Libelle_type TypePanne,
              a.NbPanne,
              case 
               when a.DureePanne<0 then -a.DureePanne
               else a.DureePanne
              end as  DureePanne,
              clasification_machi
              from FactPanne a
              left join DimDate b on b.IdDateSK=a.IdDateSK
              left join DimUsine c on c.IdUsineSK=a.IdUsineSK
              left join DimTypePanne d on d.IdTypePanneSK=a.IdTypePanneSK      
              where b.Year= cast(? as varchar) and c.IdUsine = cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VIdUsine)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
}



get_dataEffectif <- function() {
  req<-"
        select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbEffectif,a.NbPermanent,
        a.NbSaisonnier,a.NbLocaux,a.NbExpat,a.NbTemporaire,a.NbGardien,d.VALL as Direction,
        a.NbFonctAffecte,a.NbFonctDetache,a.NbCadre,a.NbAgm,a.NbEmp,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) <=20 then 1
          else 0
          end as de0a20An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=21 and DATEDIFF(year,dtna,SYSDATETIME()) <=30 then 1
          else 0
        end as de21a30An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=31 and DATEDIFF(year,dtna,SYSDATETIME()) <=40 then 1
          else 0
        end as de31a40An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=41 and DATEDIFF(year,dtna,SYSDATETIME()) <=50 then 1
          else 0
        end as de41a50An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=51 and DATEDIFF(year,dtna,SYSDATETIME()) <=60 then 1
          else 0
        end as de51a60An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=61 and DATEDIFF(year,dtna,SYSDATETIME()) <=70 then 1
          else 0
          end as de61a70An,
        case
          when DATEDIFF(year,dtna,SYSDATETIME()) >=71 then 1
          else 0
        end as plus70An,

        DATEDIFF(year,dtna,SYSDATETIME()) as Age,
        e.VALL as MotifDepart,
        DATEDIFF(year,dtes,SYSDATETIME()) as Anc
                from FactEffectif a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
                left join stg_RHFNOM e on e.cacc=c.MTFR
                          and d.ctab='23' and d.nume=1
        where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)

}

get_dataEffectifCumul <- function() {
  req<-"
        select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbEffectif,
        a.NbPermanent,a.NbSaisonnier,a.NbLocaux,a.NbExpat,a.NbTemporaire,a.NbGardien,
        d.VALL as Direction,
        a.NbFonctAffecte,a.NbFonctDetache,a.NbCadre,a.NbAgm,a.NbEmp
                from FactEffectif a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
        where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataDepart <- function() {
  req<-"
        select
       b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbDepart,a.NbPermanent,
        a.NbSaisonnier,a.NbTemporaire,a.NbGardien,
        d.VALL as Direction, e.VALL as MotifDepart
                from FactDepart a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
                left join stg_RHFNOM e on e.cacc=c.MTFR
                          and e.ctab='23' and e.nume=1
        where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataDepartCumul <- function() {
  req<-"
        select
       b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbDepart,a.NbPermanent,
        a.NbSaisonnier,a.NbTemporaire,a.NbGardien,
        d.VALL as Direction, e.VALL as MotifDepart
                from FactDepart a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
                left join stg_RHFNOM e on e.cacc=c.MTFR
                          and e.ctab='23' and e.nume=1
        where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataRecrutement <- function() {
  req<-"
        select
       b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbRecrutement,a.NbPermanent,
        a.NbSaisonnier,a.NbTemporaire,a.NbGardien,
        d.VALL as Direction
                from FactRecrutement a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
        where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataRecrutementCumul <- function() {
  req<-"
        select
       b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbRecrutement,a.NbPermanent,
        a.NbSaisonnier,a.NbTemporaire,a.NbGardien,
        d.VALL as Direction
                from FactRecrutement a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
        where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataStock <- function() {
  req<-"
       select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month,
        c.codart,c.LIBART1,c.LIBART2, a.Qte,a.Montant from FactStock a
        left join DimDate b on b.IdDateSK=a.IdDateSK
        left join DimArticle c on c.IdArticleSK=a.IdIdArticleSK 
	      where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
	      and c.UNISTO='L'
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataStockCumul <- function() {
  req<-"
       select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month,
        c.codart,c.LIBART1,c.LIBART2, a.Qte,a.Montant from FactStock a
        left join DimDate b on b.IdDateSK=a.IdDateSK
        left join DimArticle c on c.IdArticleSK=a.IdIdArticleSK 
	      where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
	      and c.UNISTO='L'
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataAchat <- function() {
  req<-"
       select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month,
        c.NumCpt,c.NUMTIE,c.NOMFOU, a.NbCde,a.NbCdeCFA,a.NbCdeEURO,a.NbCdeUSD,
        a.MontantCdeCFA,a.MontantCdeEURO,a.MontantCdeUSD
        
        from FactAchat a
        left join DimDate b on b.IdDateSK=a.IdDateSK
        left join DimFournisseur c on c.IdFournisseurSK=a.IdFournisseurSK 
	      where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
	  "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataAchatCumul <- function() {
  req<-"
       select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month,
        c.NumCpt,c.NUMTIE,c.NOMFOU, a.NbCde,a.NbCdeCFA,a.NbCdeEURO,a.NbCdeUSD,
        a.MontantCdeCFA,a.MontantCdeEURO,a.MontantCdeUSD
        
        from FactAchat a
        left join DimDate b on b.IdDateSK=a.IdDateSK
        left join DimFournisseur c on c.IdFournisseurSK=a.IdFournisseurSK 
	      where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
	  "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataUtilisateur <- function() { #Obtention des donnees 
  req<-"select CodeUtilisateur, NomUtilisateur,MotPasse, a.ProfilUtilisateur
      from Utilisateur a
    "
  #myvalue<-data.frame(VCodeSociete)
  sqlExecute(cn, req, fetch=TRUE)
}


get_dataMission <- function() {
  req<-"
        select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbMission,a.NbjourMission,
        d.VALL as Direction, a.MontantMission
                from FactMission a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
                left join stg_RHFNOM e on e.cacc=c.MTFR
                          and d.ctab='23' and d.nume=1
       where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataMissionCumul <- function() {
  req<-"
        select
        b.Year as Periode,b.DayOfWeek,b.MonthName,
        b.Date DateOpe, b.Month, c.NMAT,c.NIV1,c.NIV2,c.NIV3,c.CLAS,
        c.NOM,PREN,SEXE,c.SITF,c.CAT,c.ECH,c.GRAD,c.FONC,  a.NbMission,a.NbjourMission,
        d.VALL as Direction, a.MontantMission
                from FactMission a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                left join DimEmploye c on c.IdEmployeSK=a.IdEmployeSK
                left join stg_RHFNOM d on d.cacc=c.niv1
                          and d.ctab='1' and d.nume=1
                left join stg_RHFNOM e on e.cacc=c.MTFR
                          and d.ctab='23' and d.nume=1
       where b.Year= cast(? as varchar) and b.Month<=cast(? as varchar)
    "
  myvalue<-data.frame(VYear,VMonth)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}

get_dataFacture <- function() {
           V_year<-VYear
           V_Month<-VMonth
           
           req<-"    
           select 
                b.Year as Periode,b.DayOfWeek,b.MonthName,
                b.Date DateOpe, b.Month,
                a.NbFacture,a.PoidsNet,a.NbBalle
                from FactFacture a
                left join DimDate b on b.IdDateSK=a.IdDateSK
                 where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
                
          "  
           myvalue<-data.frame(V_year,V_Month)
           #myvalue<-data.frame(Year,input$Month)
           sqlExecute(cn, req,myvalue, fetch=TRUE)
           
}

get_dataFactureDef <- function() {
  V_Year<-VYear
  V_Month<-VMonth
  
  req<-"    
           select 
            b.Year as Periode,b.DayOfWeek,b.MonthName,
            b.Date DateOpe, b.Month,
            dt_factur_def,
            nm_negoci,
            li_pays,
            rf_contrats_sdcc,
            rf_contrats_negoci,
            references_instru,
            rf_bl,
            dt_bl,
            recolte,
            type as typeF,
            pu_euro,
            li_incote,
            nb_balle,
            montant_contrat,
            montant_tot_fact_def,
            rf_factur_def
            from stg_v_facture_definitive a
            INNER join  DimDate b 
            	ON CONVERT(varchar,b.Date,23) = CONVERT(varchar,a.dt_factur_def,23)
               where b.Year= cast(? as varchar) and b.Month=cast(? as varchar)
          "  
  myvalue<-data.frame(V_Year,V_Month)
  sqlExecute(cn, req,myvalue, fetch=TRUE)
  
}


#===================================================================================================================
#---Fermeture de la connexion bd-----------------------------------------------------------------
#===================================================================================================================
close_connection <- function() {
    odbcClose(cn)
}

jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

#===================================================================================================================
#========= DEBUT DESIGN TABLE DE BORD==============================================================================
#===================================================================================================================

#---Dashboard header carrying the title of the dashboard---------------------
header <- dashboardHeader(title = "TABLEAU DE BORD - SODECOTON")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(

    useShinyjs(html = FALSE),
    extendShinyjs(text = jsCode, functions = c("winprint")),
    shinyjs::hidden(
        div(id = "Sidebar", 
            
            sidebarMenu(
                menuItem("Source - SODECOTON ", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("SELECTION PARAMETRES",badgeColor = "green"),
                selectInput("Year","Annee",choices=c(2017,2018,2019, 2020,2021,2022,2023),selected = 2020),
                selectInput("Month","Mois",choices=NULL),
                selectInput("Usine","Usine",choices=NULL),
                selectInput("Huilerie","Huilerie",choices=NULL),
                actionButton("ScreenDownload", "Telecharger")
                
            )
        ))
)

#--------Synthese-------------------------------------------------------------
frow1Synhese <- fluidRow(
  valueBoxOutput("value1Synthese",width = 2)
  ,valueBoxOutput("value4Synthese",width = 2)
  #,valueBoxOutput("value4",width = 2)
  #,valueBoxOutput("value5",width = 2)
  #,valueBoxOutput("value12",width = 2)
  #,valueBoxOutput("value10",width = 2)
)
#----------------------------------------------------------------------------

#--------------------------UI Huilerie---------------------------------------
frowH1 <- fluidRow(
   valueBoxOutput("TauxHnGr", width = 2)
  ,valueBoxOutput("TauxTxGr", width = 2)
  ,valueBoxOutput("ProductiviteDiamaor", width = 2)
)

frowH2 <- fluidRow(
   valueBoxOutput("QteGraineRecep", width = 2)
  ,valueBoxOutput("QteGraineMO", width = 2)
  ,valueBoxOutput("NbSacsTotal", width = 2)
  ,valueBoxOutput("QteHuileNProduitLT", width = 2)
)

frowH3 <- fluidRow(
  box(width = 6,
      title = "EVOLUTION DES RECEPTION DES GRAINES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChart4Output("CourbeReceptionGraine", height = "290px")
  ),
  box(width = 6,
      title = "RECEPTION DES GRAINES PAR TRANCHE HORAIRE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChartsOutput("RecepGraineByTranche",height = "290px")
  ),
  
)


frowH4 <- fluidRow(
  box(width = 6,
      title = "EVALUATION DES MISES EN OEUVRE PAR QUART"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChartsOutput("MoByQuart", height = "290px")
  ),
  box(width = 6,
      title = "EVALUATION DES MISES EN OEUVRE PAR TRANCHE HORAIRE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChartsOutput("MoByTranche",height = "290px")
  ),
  
)

frowH5 <- fluidRow(
  box(width = 6,
      title = "PRODUCTION DU TOURTEAU PAR QUART"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChartsOutput("TtxByQuart", height = "290px")
  ),
  box(width = 6,
      title = "PRODUCTION DE L'HUILE NEUTRE PAR QUART"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,amChartsOutput("HnByQuart",height = "290px")
  ),
  
)
#---------------FIN UI Huilerie--------------------------------------------

#----------------UI Informatique-------------------------------------------
PremiereLigne_Informatique <- fluidRow(
  valueBoxOutput("NombreIncident", width = 2)
  ,valueBoxOutput("NombreProjet", width = 2)
  ,valueBoxOutput("QuantiteConsommable", width = 2)
  
)


DeuxiemeLigne_Informatique <- fluidRow(
   valueBoxOutput("TauxRespectDelaiProjet", width = 2)
  ,valueBoxOutput("TauxSatisfactionUtilisateur", width = 2)
  ,valueBoxOutput("TauxParticipation", width = 2)
)


#----------------FIN UI Informatique-------------------------------------------



frow1 <- fluidRow(
  valueBoxOutput("value1",width = 2)
  ,valueBoxOutput("value2",width = 2)
  ,valueBoxOutput("value4",width = 2)
  ,valueBoxOutput("value5",width = 2)
  ,valueBoxOutput("value12",width = 2)
  ,valueBoxOutput("value10",width = 2)
)
    

frow2 <- fluidRow( 
    box(width = 6,
        title = "POIDS MOYENS DES BALLE (KG) PAR EQUIPE - MOIS M vs M-1"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,DTOutput("TabBallesCompare",height = "290px",width = "80%")
    ),
    box(width = 6,
        title = "PERFORMANCE DES EQUIPES"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = FALSE 
        ,plotlyOutput("GNbBallesEquipes", height = "290px")
    )#,
    #selectInput("Nature","Nature",choices = c("Poids Moyen","Rendement Fibre","Egrene Moy/J"))
    
)

frow3 <- fluidRow(
    box(width = 6,
       title = "RENDEMENT FIBRE"
       ,status = "primary"
       ,solidHeader = TRUE 
       ,collapsible = TRUE 
       ,plotlyOutput("GRendementFibre", height = "290px")
    ),
    box(width = 6,height=10,
        title = "RENDEMENT JOURNALIER - CG et FIBRES"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,DTOutput("TabRendement",height = "290px",width = "80%")
    ),

)

frow4 <- fluidRow(
  valueBoxOutput("value3",width = 2),
  valueBoxOutput("value6",width = 2),
  
  valueBoxOutput("value7",width = 2),
  valueBoxOutput("value8",width = 2),
  valueBoxOutput("value9",width = 2),
  valueBoxOutput("value11",width = 2)
)

frow5 <- fluidRow(
  valueBoxOutput("value7",width = 2)
  
)

frow6 <- fluidRow(
  valueBoxOutput("value13",width = 2),
  valueBoxOutput("value14",width = 2),
  valueBoxOutput("value15",width = 2),
  valueBoxOutput("value16",width = 2),
  valueBoxOutput("value17",width = 2),
  valueBoxOutput("value18",width = 2)
  
)


frow7 <- fluidRow(
  box(width = 4,
      title = "TAUX FIBRE PAR CATEGORIE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabtauxFibre",height = "290px",width = "100%")
  ),
  #box(width = 6,
  #    title = "PERFORMANCE DES EQUIPES"
  #    ,status = "primary"
  #    ,solidHeader = TRUE 
  #    ,collapsible = FALSE 
  #    ,plotlyOutput("GTauxFibreSoie", height = "290px")
  #)#,
  box(width = 3,
      title = "TAUX FIBRE PAR CATEGORIE (%)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GTauxFibreCategorie", height = "300px")
  ),
  box(width = 5,
      title = "TONNAGE FIBRE JOURNALIER / CATEGORIE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GTauxFibreCategorie2", height = "300px")
  )
  
)

frow8 <- fluidRow(
  valueBoxOutput("value19",width = 4),
  valueBoxOutput("value20",width = 4),
  valueBoxOutput("value21",width = 4)
)

frow9 <- fluidRow(
  box(width = 6,
      title = "TAUX FIBRES / SOIES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("LongueurSoie",height = "290px",width = "80%")
  ),
  box(width = 6,
      title = "TAUX FIBRES / VARIETES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabtauxFibreVariete",height = "290px",width = "80%")
  )
)

frow10 <- fluidRow(
  box(width = 12,
      title = "CLASSEMENT - JOURNALIER"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabclassementJour",height = "290px",width = "80%")
  )
)

frow11 <- fluidRow(
  valueBoxOutput("value22",width = 3),
  valueBoxOutput("value30",width = 3),
  valueBoxOutput("value31",width = 2),
  valueBoxOutput("value32",width = 2),
  valueBoxOutput("value33",width = 2)
  #valueBoxOutput("value23",width = 3),
  #valueBoxOutput("value24",width = 3),
  #valueBoxOutput("value25",width = 3)
)
frow12 <- fluidRow( #--Cumul Annuel
  valueBoxOutput("value26",width = 3),
  valueBoxOutput("value34",width = 3),
  valueBoxOutput("value35",width = 2),
  valueBoxOutput("value36",width = 2),
  valueBoxOutput("value37",width = 2)
  #valueBoxOutput("value27",width = 3),
  #valueBoxOutput("value28",width = 3),
  #valueBoxOutput("value29",width = 3)
)

frow12b <- fluidRow(
  valueBoxOutput("value48",width = 3),
  valueBoxOutput("value49",width = 3),
  valueBoxOutput("value50",width = 2),
  valueBoxOutput("value51",width = 2),
  valueBoxOutput("value52",width = 2)
)

frow13 <- fluidRow(
  box(width = 6,
      title = "EVOLUTION DES EFFECTIFS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionEffectif", height = "290px")
  ),
  box(width = 6,height=10,
      title = "EVOLUTION DES EFFECTIFS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabEffectif",height = "290px",width = "100%")
  )
)

frow14 <- fluidRow(
  #box(width = 5,
  #    title = "REPARTITION DES EFFECTIFS"
  #    ,status = "primary"
  #    ,solidHeader = TRUE 
  #    ,collapsible = TRUE 
  #    ,plotlyOutput("GEffectifparSexe", height = "290px")
  #),
  box(width = 3,
      title = "EFFECTIFS PAR CATEGORIES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabEffectifCatSocio",height = "320px",width = "100%")
  ),
  box(width = 9,
      title = "REPARTITION DES EFFECTIFS DU MOIS PAR DIRECTION"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabEffectifParDirection",height = "320px",width = "100%")
  )
  
)

frow15 <- fluidRow(
  box(width = 6,
      title = "EVOLUTION MENSUELLE DES EFFECTIFS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabEffectifParClasse",height = "290px",width = "100%")
  ),
  box(width = 6,
      title = "EVOLUTION MENSUELLE DES EFFECTIFS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionClasseEffectif", height = "290px")
  )
)

frow15b <- fluidRow(
  box(width = 4,
      title = "Effectifs des permanents par age"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAgeSexePermanent", height = "400px")
  ),
  box(width = 4,
      title = "Effectifs des Saisonniers par age"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAgeSexeSaisonnier", height = "400px")
  ),
  box(width = 4,
      title = "Effectifs des temporaire par age"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAgeSexeTemporaraire", height = "400px")
  )
  
)


frow15c <- fluidRow(
  box(width = 4,
      title = "Effectifs des permanents par Ancienneté"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAncSexePermanent", height = "400px")
  ),
  box(width = 4,
      title = "Effectifs des Saisonniers par Ancienneté"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAncSexeSaisonnier", height = "400px")
  ),
  box(width = 4,
      title = "Effectifs des temporaire par Ancienneté"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPyramideAncSexeTemporaraire", height = "400px")
  )
  
)


frow16 <- fluidRow(
  box(
      rpivotTableOutput("MatriceEffectif")
  )
)

frow17 <- fluidRow(
  valueBoxOutput("value38",width = 3),
  valueBoxOutput("value39",width = 3),
  valueBoxOutput("value40",width = 2),
  valueBoxOutput("value41",width = 2),
  valueBoxOutput("value42",width = 2)
)

frow18 <- fluidRow(
  box(width = 7,
      title = "REPARTITION DES DEPARTS PAR DIRECTION"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabDepartParDirection",height = "290px",width = "90%")
  ),
  box(width = 5,
      title = "EVOLUTION MENSUELLE DES DEPARTS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabDepartParClasse",height = "290px",width = "80%")
  )
)

frow18b <- fluidRow(
  box(width = 6,
      title = "DEPARTS DU MOIS - REPARTITION PAR MOTIF"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabDepartParMotif",height = "290px",width = "90%")
  ),
  box(width = 6,
      title = "DEPARTS CUMUL ANNUEL- REPARTITION PAR MOTIF"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabDepartParMotifCumul",height = "290px",width = "90%")
  )
)

frow19 <- fluidRow(
  valueBoxOutput("value43",width = 3),
  valueBoxOutput("value44",width = 3),
  valueBoxOutput("value45",width = 2),
  valueBoxOutput("value46",width = 2),
  valueBoxOutput("value47",width = 2)
)
frow20 <- fluidRow(
  box(width = 7,
      title = "REPARTITION DES RECRUTEMENTS PAR DIRECTION"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabRecrutementParDirection",height = "290px",width = "90%")
  ),
  box(width = 5,
      title = "EVOLUTION MENSUELLE DES RECRUTEMENTS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabRecrutementParClasse",height = "290px",width = "80%")
  )
)

frow21 <- fluidRow(
  #valueBoxOutput("value53",width = 3),
  valueBoxOutput("value53Vte",width = 3),
  valueBoxOutput("value53VtePoidsNet",width = 3),
  valueBoxOutput("value53VteNbBalle",width = 3),
  valueBoxOutput("value53VteMontant",width = 3)
)

frow21c <- fluidRow(
  valueBoxOutput("value53NbContrat",width = 3),
  valueBoxOutput("value53NbClient",width = 3),
  valueBoxOutput("value53PUMin",width = 3),
  valueBoxOutput("value53PUMax",width = 3)
)

frow21a <- fluidRow(
  box(width = 6,
      title = "REPARTITION DES MONTANTS PAR TYPE DE FIBRE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabtauxVente",height = "290px",width = "90%")
  ),
  box(width = 6,
      title = "REPARTITION MONTANTS PAR TYPE DE FIBRE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GTauxVenteParType", height = "300px")
  )
)

frow21b <- fluidRow(
  box(width = 6,
      title = "REPARTITION DES MONTANTs PAR CLIENT"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabtauxVenteParClient",height = "290px",width = "90%")
  ),
  box(width = 6,
      title = "REPARTITION MONTANTS PAR CLIENT"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GTauxVenteParClient", height = "300px")
  )
)



frow22 <- fluidRow(
  box(width = 7,height = "270px",
      h3("Evolution mensuelle Qte Stock (Litre)"),
  pivottablerOutput("MatriceStock2")
  )
  #pivottablerOutput("ConsultationMvtSaisie_test")
  
  #box(width = 6,
  #    title = "EVOLUTION QUANTITE ET MONTANT PAR PRODUIT"
  #    ,status = "primary"
  #    ,solidHeader = TRUE 
  #    ,collapsible = TRUE 
  #    ,tableOutput("TabStockEvolution2")
  #)
)

frow23 <- fluidRow(
  box(width = 4,
      title = "QUANTITE ET MONTANT PAR PRODUIT"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabQteMontantParProduit",height = "290px",width = "90%")
  )
)

frow24 <- fluidRow(
  valueBoxOutput("value55",width = 3),
  valueBoxOutput("value56",width = 1),
  valueBoxOutput("value57",width = 1),
  valueBoxOutput("value58",width = 3),
  valueBoxOutput("value59",width = 2),
  valueBoxOutput("value60",width = 2)
)
frow25 <- fluidRow(
  valueBoxOutput("value61",width = 3),
  valueBoxOutput("value62",width = 1),
  valueBoxOutput("value63",width = 1),
  valueBoxOutput("value64",width = 3),
  valueBoxOutput("value65",width = 2),
  valueBoxOutput("value66",width = 2)
)

frow26 <- fluidRow(
  box(width = 6,
      title = "EVOLUTION MENSUELLE NOMBRE DE COMMANDES CRÉES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabCdeEvolution",height = "290px",width = "100%")
  ),
  box(width = 6,
      title = "EVOLUTION MENSUELLE NOMBRE DE COMMANDES CRÉES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionNbCde", height = "290px")
  )
)

frow27 <- fluidRow(
  box(width = 6,
      title = "EVOLUTION MENSUELLE DES MONTANS DE COMMANDES CRÉES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabMonantCdeEvolution",height = "290px",width = "100%")
  ),
  box(width = 6,
      title = "EVOLUTION MENSUELLE DES MONTANS DE COMMANDES CRÉES"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionCdeMontant", height = "290px")
  )
)

frow28 <- fluidRow(
  #box(width = 6,
  #    title = "EVOLUTION MENSUELLE DES MONTANS DE COMMANDES CRÉES"
  #    ,status = "primary"
  #    ,solidHeader = TRUE 
  #    ,collapsible = TRUE 
  #    ,DTOutput("TabMonantCdeEvolution",height = "290px",width = "100%")
  #),
  box(width = 6,
      title = "MONTANT DES COMMANDES PAR DATE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GMontantCdeParDate", height = "290px")
  ),
  box(width = 6,
      title = "MONTANT DES COMMANDES PAR FOURNISSEUR"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GMontantCdeParFsseur", height = "290px")
  )
)

frow29 <- fluidRow(
  box(width = 8,
      title = "REPARTITION DES EFFECTIFS PAR TRANCHE D'AGE"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabEffectifParAge",height = "290px",width = "90%")
  ),
  box(width = 1,
      h4("choisir une plage d'age pour les effectifs"),
      numericInput("AgeDebut","Age debut",value = 0,width = "150px"),
      numericInput("AgeFin","Age fin",value = 0,width = "150px"),
  ),
  box(width = 3,
      title = "REPARTITION DES EFFECTIFS PAR TRANCHE D'AGE",
      status = "primary",
      DTOutput("TabEffectifParAge2",height = "200px",width = "90%")
  )
  #box(width = 5,
  #    title = "EVOLUTION MENSUELLE DES RECRUTEMENTS"
  #    ,status = "primary"
  #    ,solidHeader = TRUE 
  #    ,collapsible = TRUE 
  #    ,DTOutput("TabRecrutementParClasse",height = "290px",width = "80%")
  #)
)

frow30 <- fluidRow(
  valueBoxOutput("value67",width = 4),
  valueBoxOutput("value68",width = 4),
  valueBoxOutput("value69",width = 4)#,
  #valueBoxOutput("value70",width = 3)
)
frow31 <- fluidRow(
  box(width = 5,
      title = "EVOLUTION MENSUELLE DES MISSIONS"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabMissionParMois",height = "290px",width = "90%")
  ),
  box(width = 7,
      title = "CUMUL ANNUEL DES MISSIONS PAR DIRECTION"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabMissionParDirection",height = "290px",width = "100%")
  )
)

frow32 <- fluidRow(
  box(width = 8,
      title = "COUTS DES MISSIONS PAR DIRECTION"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GMissionParDirection", height = "400px")
  ),
  box(width = 4,
      title = "EVOLUTION JOURNALIERE DES FRAIS MISSION "
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionMissionParMois", height = "400px")
  )
)

frow33 <- fluidRow(
  valueBoxOutput("value71",width = 3),
  valueBoxOutput("value72",width = 3),
  valueBoxOutput("value73",width = 3),
  valueBoxOutput("value74",width = 3)
)
frow100 <- fluidRow(
  #valueBoxOutput("value100",width = 3),
  #valueBoxOutput("value101",width = 3),
  valueBoxOutput("value102",width = 2),
  valueBoxOutput("value103",width = 2),
  valueBoxOutput("value104",width = 2),
  valueBoxOutput("value105",width = 2),
  valueBoxOutput("value106",width = 2),
  valueBoxOutput("value107",width = 2)
)

frow102 <- fluidRow(
  box(width = 4,
      title = "RUBRIQUE DES TEMPS (Heures)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabTemps2",height = "200px",width = "90%")
  ),
  box(width = 4,
      title = "RUBRIQUE DES TEMPS (Heures)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabTemps3",height = "200px",width = "90%")
  ),
  box(width = 4,
      title = "RUBRIQUE DES TEMPS (Heures)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabTemps4",height = "200px",width = "90%")
  )
)


frow101 <- fluidRow(
  box(width = 3,
      title = "EVOLUTION MENSUELLE TEMPS NET (Heures)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,DTOutput("TabTempsNetEvolution",height = "290px",width = "90%")
  ),
  box(width = 8,
      title = "EVOLUTION MENSUELLE TEMPS NET (Heures)"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionTempsNetParMois", height = "290px")
  )
)


frow34 <- fluidRow(
  box(width = 8,
      title = "NOMBRE DE PANNES PAR TYPE DE PANNE - CUMUL ANNUEL"
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GPanneParType", height = "400px")
  ),
  box(width = 4,
      title = "EVOLUTION JOURNALIERE DE  NOMBRE DE PANNES "
      ,status = "primary"
      ,solidHeader = TRUE 
      ,collapsible = TRUE 
      ,plotlyOutput("GEvolutionPannesParMois", height = "400px")
  )
)

frow35 <- fluidRow(
  box(width = 6,height = "600px",
      h3("Evolution mensuelle nombre de pannes"),
      pivottablerOutput("MatricePanne")
  ),
  box(width = 6,height = "600px",
      h3("Evolution mensuelle duree des pannes (en Heures)"),
      pivottablerOutput("MatriceDureePanne")
  )
  
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  #actionButton("ScreenDownload", "Telecharger"),
    tabsetPanel(
      
        tabPanel(tags$h3("SODECOTON - SUIVI DES INDICATEURS DE PERFORMANCES"),
                 tags$h4("veuillez vous connecter"),
                 
                 tags$br(),
                 
                 value = 1,
                 br(),
                 
                   div(id = "Connexion",
                 textInput("username", "Username"),
                 passwordInput("password", label = "Password"),
                 actionButton("login", "Login",class = "btn-primary"),
                 textOutput("pwd")
                   ),
                 br(),
                 br(),
                 img(src="Sodecoton.jpg",height="5%", width="40%")
                 ),
        id = "tabselected", type = "pills"
        )
    
    
    
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'TABLEAUX DE BORD DE LA SODECOTON', header, sidebar, body, skin='red')




#=================== Server =======================================================
Server <- function(input, output, session) { 
    
    # Run every 30 seconds
    
    dataf_production = reactive({
        invalidateLater(900000,session)
        input$Year
        input$Usine
        input$Month
        get_dataProduction()
    }
    )
    
    dataf_productionUsine = reactive({
        invalidateLater(900000,session)
        input$Year
        input$Usine
        input$Month
        get_dataProductionUsine()
    }
    )
    
    dataf_evacuation = reactive({
        invalidateLater(900000,session)
        input$Year
        input$Usine
        input$Month
        get_dataEvacuation()
    }
    )
    
    dataf_classement = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Usine
      input$Month
      get_dataClassement()
    }
    )
    
    dataf_Panne = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Usine
      input$Month
      get_dataPanne()
    }
    )
    
    dataf_PanneCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Usine
      input$Month
      get_dataPanneCumul()
    }
    )
    dataf_PanneCumul12Mois = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Usine
      input$Month
      get_dataPanneCumul12Mois()
    }
    )
    
    dataf_Effectif = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataEffectif()
    }
    )
    dataf_EffectifCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataEffectifCumul()
    }
    )
    dataf_Depart = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataDepart()
    }
    )
    dataf_DepartCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataDepartCumul()
    }
    )
    
    dataf_Recrutement = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataRecrutement()
    }
    )
    dataf_RecrutementCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataRecrutementCumul()
    }
    )
    
    dataf_Stock = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataStock()
    }
    )
    
    dataf_StockCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataStockCumul()
    }
    )
    
    dataf_Achat = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataAchat()
    }
    )
    
    dataf_AchatCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataAchatCumul()
    }
    )
    
    dataf_Mission = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataMission()
    }
    )
    dataf_MissionCumul = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataMissionCumul()
    }
    )

    
    dataf_facture = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataFacture()
    }
    )
    
    dataf_factureDef = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Month
      get_dataFactureDef()
    }
    )
    
    #---get data for huilerie------------
    dataf_Huilerie = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataHuilerie()
    })
    
    dataf_Huilerie_param = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataHuilerie_param()
    })
    #--------All datas get for Huilerie----------
    #---get data for Informatique------------
    dataf_Incident_param = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataIncident()
    })
    
    dataf_Projet = reactive({
      invalidateLater(900000,session)
      get_dataProjet()
    })
    
    dataf_Projet_param = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataProjetParam()
    })
    
    dataf_Consommable_param = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataConsommable()
    })
    
    dataf_Satisfaction_param = reactive({
      invalidateLater(900000,session)
      input$Year
      input$Huilerie
      input$Month
      get_dataSatisfaction()
    })
    #--------All datas get for Informatique----------
    
    #--- On recupere toutes les huileries--------
    reqHuilerie<-
      sqlQuery(cn," select ' ' CodeUsine,' ' NomUsine
                                UNION ALL
                                select CodeUsine, NomUsine from DimUsineHuilerie
                              "
      )
    #----- On charge le combo des huileries-------------
    choicesHuilerie = setNames(reqHuilerie$CodeUsine,reqHuilerie$NomUsine)
    updateSelectInput(session, 'Huilerie', choices=choicesHuilerie, selected = 1)
    
    
    reqUsine<-
        sqlQuery(cn," select ' ' IdUsine,' ' NomUsine
                                UNION ALL
                                select IdUsine,Libelle from DimUsine
                              "
        )
    reqMois<-
        sqlQuery(cn,"  SELECT distinct Month,MonthName  FROM [DimDate] order by Month "
        )
    
    choicesUsine = setNames(reqUsine$IdUsine,reqUsine$NomUsine)
    updateSelectInput(session,'Usine',choices=choicesUsine,selected = 5)
    
    choicesMois = setNames(reqMois$Month,reqMois$MonthName)
    updateSelectInput(session,'Month',choices=choicesMois,selected = 5)
    
    
    user_vec <- c("user" = "user",
                  "stalle" = "coton2020"
                  )
    
    
    #----On capture tous les changements de valeurs dans la liste deroulante sur les huileries-----
    observeEvent(input$Huilerie, {
      VIdHuilerie <<-input$Huilerie
    })
    
    observeEvent(input$Year, {
        VYear<<-input$Year
        #runjs('Shiny.setInputValue("Usine", " ", {priority: "event"});')
    })
    observeEvent(input$Usine, {
        VIdUsine<<-input$Usine
    })
    observeEvent(input$Month, {
        VMonth<<-input$Month
        VMonthPrec<<-as.numeric(VMonth)-1
        VMonthPrec<-as.character(VMonthPrec)
    })
    
    observeEvent(input$tabselected, {
      if (input$tabselected == "RESSOURCES H." 
          | input$tabselected == "VENTES & STOCKS" 
          | input$tabselected=="ACHATS & FINANCES" 
          | input$tabselected=="SYNTHESE" 
          ) {
        shinyjs::hide("Usine")
        shinyjs::hide("Huilerie")
      }else if (input$tabselected == "HUILERIES"){
        shinyjs::hide("Usine")
        shinyjs::show("Huilerie")
      }
      else{
        shinyjs::show("Usine")
        shinyjs::hide("Huilerie")
      }
      
    })
    
    observeEvent(input$login, {
      
      Vusername<<-input$username
      req<-"select * from utilisateur where CodeUtilisateur like cast(? as varchar) and MotPasse like cast(? as varchar)"
      myvalue<-data.frame(Vusername,input$password)
      user<-sqlExecute(cn, req, myvalue,fetch=TRUE)
      
      nb<-nrow(user)
      #if(nb!=0){
      #  alert("Mot de passe incorectxxx!");
        
      
      #alert(names(user_vec[1]))
        #if (str_to_lower(input$username) %in% names(user_vec)) 
      if (nb!=0)   
          { 
            if (input$password == user$MotPasse) {
                shinyjs::show(id = "Sidebar")
                shinyjs::hide(id = "Connexion")
                
                
                output$report <- downloadHandler(
                  filename = "report.html",
                  content = function(file) {
                    tempReport <- file.path(tempdir(), "report.Rmd")
                    file.copy("report.Rmd", tempReport, overwrite = TRUE)
                    params <- list(a = value1G, b=input$Year, c=input$Month)
                    rmarkdown::render(tempReport, output_file = file,
                                      params = params,
                                      envir = new.env(parent = globalenv())
                    )
                  }
                )
                
                #--------------SYNTHESE---------------------------------------------
                output$value1Synthese <- renderValueBox({
                  dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                  dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & Month==input$Month)
                  
                  Valeur1 <- sum(dataf_productionUsine$PdsCgEgrene)/1000
                  #value1G<<-Valeur1
                  valueBox(formatC(Valeur1,digits = 0,format ="f",big.mark=' ' ),'TonnageCgEgrene(t)',color = "green")
                  
                })
                
                output$value4Synthese <- renderValueBox({
                  dataf_production<-dataf_production()[complete.cases(dataf_production()$NbBalles),]
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & Month==input$Month)
                  Valeur4 <- sum(dataf_production$NbBalles)
                  valueBox(formatC(Valeur4,digits = 0,format ="f",big.mark=' ' ),'Nombre de Balls',color = "green")
                })
                
                #-------------------------------------------------------------------
                
                #---------------Chargement des widgets HUILERIES---------------------------
                
                
                
                #---------Premiere ligne page huileries : Ligne des Kpis------
                        #-- Kpi Taux Rendement huile neutre / graine
                output$TauxHnGr<-renderValueBox({
                  dataf_Huilerie<-get_dataHuilerie()[complete.cases(get_dataHuilerie()$HuileNProduitLT),]
                  dataf_Huilerie<-dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie & Month==input$Month)
                  QteHuileNProduite<-sum(dataf_Huilerie$HuileNProduitLT)
                  QteGraineMoKG<-sum(dataf_Huilerie$GraineMoKG)
                  
                  TauxHnGr<<-0
                  if(QteGraineMoKG != 0){
                    TauxHnGr = (QteHuileNProduite / QteGraineMoKG) * 100
                  }
                  valueBox(formatC(TauxHnGr, digits = 2, format ="f",big.mark=' ' ), 'Rend. HN sur Graine(%)', color = "green")
                  
                })
                          #-- Kpi Taux Rendement Tourteau / graine
                output$TauxTxGr<-renderValueBox({
                  dataf_Huilerie<-get_dataHuilerie()[complete.cases(get_dataHuilerie()$AlibetProduitSac),]
                  dataf_Huilerie<-dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie & Month==input$Month)
                  QteAlibetProduitSac<-sum(dataf_Huilerie$AlibetProduitSac)
                  QteFarine21ProduitSac<-sum(dataf_Huilerie$Farine21ProduitSac)
                  QteGraineMoKG <- sum(dataf_Huilerie$GraineMoKG)
                  
                  TauxTxGr<<-0
                  if(QteGraineMoKG != 0){
                    TauxTxGr = ( (QteAlibetProduitSac + QteFarine21ProduitSac) / QteGraineMoKG) * 100
                  }
                  valueBox(formatC(TauxTxGr, digits = 2, format ="f", big.mark=' ' ), 'Rend. Tourteaux sur Graine(%)', color = "green")
                  
                })
                          #-- Kpi Productivite Diamaor
                output$ProductiviteDiamaor<-renderValueBox({
                  dataf_Huilerie<- get_dataHuilerie()[complete.cases(get_dataHuilerie()$NbCartonDiamaorU),]
                  dataf_Huilerie<-dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  ProductiviteDiamaor<-sum(dataf_Huilerie$NbCartonDiamaorU) 
                  valueBox(formatC(ProductiviteDiamaor, digits = 0, format ="f", big.mark=' ' ), 'Productivite Diamaor(U)', color = "green")
                  
                })
                
                #---------Deuxieme ligne page huileries : Ligne des autres mesures utiles-------------
                          #---- Mesure : Graine Receptionnee
                output$QteGraineRecep <- renderValueBox({
                  dataf_Huilerie <- get_dataHuilerie()[complete.cases(get_dataHuilerie()$GraineRecepKG),]
                  dataf_Huilerie <- dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  QteGraineRecep <- sum(dataf_Huilerie$GraineRecepKG) / 1000
                  QteGraineRecepG<<-QteGraineRecep
                  valueBox(formatC(QteGraineRecep, digits = 2, format ="f",big.mark=' ' ), 'Graine Receptionnee(T)', color = "yellow")
                  
                })
                          #---- Mesure : Graine Mise en oeuvre
                output$QteGraineMO <- renderValueBox({
                  dataf_Huilerie <- get_dataHuilerie()[complete.cases(get_dataHuilerie()$GraineMoKG),]
                  dataf_Huilerie <- dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  QteGraineMO <- sum(dataf_Huilerie$GraineMoKG) / 1000
                  valueBox(formatC(QteGraineMO, digits = 2, format ="f", big.mark=' ' ), 'Graine M.O.(T)', color = "yellow")
                  
                })
          
                            #---- Mesure : Nombre de Sacs de tourteaux
                output$NbSacsTotal <- renderValueBox({
                  dataf_Huilerie <- get_dataHuilerie()[complete.cases(get_dataHuilerie()$AlibetProduitSac),]
                  dataf_Huilerie <- dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  QteAlibetProduitSac <- sum(dataf_Huilerie$AlibetProduitSac)
                  
                  dataf_Huilerie <- get_dataHuilerie()[complete.cases(get_dataHuilerie()$Farine21ProduitSac),]
                  dataf_Huilerie <- dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  NbSacFarine21 <- sum(dataf_Huilerie$Farine21ProduitSac)
                  
                  NbSacsTotal<<-QteAlibetProduitSac + NbSacFarine21
                  valueBox(formatC(NbSacsTotal, digits = 0, format ="f", big.mark=' ' ), 'Sacs de tourteaux(U)', color = "yellow")
                  
                })
                            #---- Mesure : Huile neutre produite
                output$QteHuileNProduitLT <- renderValueBox({
                  dataf_Huilerie <- dataf_Huilerie_param()[complete.cases(get_dataHuilerie()$HuileNProduitLT),]
                  dataf_Huilerie <- dataf_Huilerie %>% filter(Periode == input$Year & CodeUsine == input$Huilerie  & Month==input$Month)
                  QteHuileNProduitLT <- sum(dataf_Huilerie$HuileNProduitLT) 
                  valueBox(formatC(QteHuileNProduitLT, digits = 2, format ="f", big.mark=' ' ), 'Huile neutre produite(L)', color = "yellow")
                  
                })
                      
                #----------DIAGRAMME EVOLUTION DES RECEPTIONS DE GRAINE------------
                
                output$CourbeReceptionGraine <- renderAmChart4({
                  
                  dataf_Huilerie_<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  dataf_Huilerie_<-dataf_Huilerie_ %>% group_by(Jour) %>% summarize(GraineRecepKG=sum(GraineRecepKG))
                  
                  
                  dat <- data.frame(
                    Jour = dataf_Huilerie_$Jour,
                    Quantite = dataf_Huilerie_$GraineRecepKG
                  )  
                    amLineChart(data = dat, data2 = NULL, xValue = "Jour", yValues = "Quantite", draggable = TRUE)
                })
                
                
                
                        ##----------Comparaison des receptions par tranche_horaire--------------
                
                output$RecepGraineByTranche <- renderAmCharts({
                  
                  dataf_Huilerie_<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  dataf_Huilerie_<-dataf_Huilerie_ %>% group_by(Equipe) %>% summarize(GraineRecepKG=sum(GraineRecepKG))
                  
                  
                  dat <- data.frame(
                    Equipe = dataf_Huilerie_$Equipe,
                    Quantite = dataf_Huilerie_$GraineRecepKG
                  )  
                  amBarplot(x = "Equipe", y = "Quantite", data = dat, depth = 15, labelRotation = -45)
                })
                  
                ##----------Evaluation des mises en oeuvre par quart--------------
                
                output$MoByQuart <- renderAmCharts({
                  
                  dataf_Huilerie_<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  dataf_Huilerie_<-dataf_Huilerie_ %>% group_by(Quart) %>% summarize(GraineMoKG=sum(GraineMoKG))
                  
                  
                  dat <- data.frame(
                    Quart = dataf_Huilerie_$Quart,
                    Quantite = dataf_Huilerie_$GraineMoKG
                  )  
                  amBarplot(x = "Quart", y = "Quantite", data = dat, depth = 20, labelRotation = -45, legend = TRUE)
                })
                
                ##----------Evaluation des mises en oeuvre par tranche horaire--------------
                output$MoByTranche <- renderAmCharts({
                  datef_huilerie_p<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  datef_huilerie_p<-datef_huilerie_p %>% group_by(Equipe) %>% summarize(GraineMoKG=sum(GraineMoKG) / 1000)
                  
                  
                  dat <- data.frame(
                    label = datef_huilerie_p$Equipe,
                    value = datef_huilerie_p$GraineMoKG
                  )
                  
                  amPie(data = dat, depth = 30, legend = TRUE)
                })
                
                    ##----------PRODUCTION TOURTEAU PAR QUART
                
                output$TtxByQuart <- renderAmCharts({
                  datef_huilerie_p<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  datef_huilerie_p<-datef_huilerie_p %>% group_by(Quart) %>% summarize(Tourteau=sum(Tourteau) * 60)
                  
                  
                  dat <- data.frame(
                       label = datef_huilerie_p$Quart,
                       value = datef_huilerie_p$Tourteau
                     )

                  amPie(data = dat, depth = 30, legend = TRUE)
                })
                
                ##----------PRODUCTION DE L'HUILE NEUTRE PAR QUART
                
                output$HnByQuart <- renderAmCharts({
                  datef_huilerie_p<-dataf_Huilerie_param()[complete.cases(dataf_Huilerie_param()),]
                  datef_huilerie_p<-datef_huilerie_p %>% group_by(Quart) %>% summarize(HuileNProduitLT=sum(HuileNProduitLT))
                  
                  
                  dat <- data.frame(
                    label = datef_huilerie_p$Quart,
                    value = datef_huilerie_p$HuileNProduitLT
                  )
                  amPie(data = dat, depth = 30, legend = TRUE)
                })
                  
                  
                                  
                #---------------FIN Chargement des widgets HUILERIES-----------------------
                
                #---------------Chargement des widgets INFORMATIQUE-----------------------
                #---------Premiere ligne page Informatique : Ligne des Kpis------
                #-- Kpi Nombre d'incident 
                output$NombreIncident<-renderValueBox({
                  dataf_Incident<-dataf_Incident_param()
                  NombreIncident_<-length(dataf_Incident$Tickets)
                  valueBox(formatC(NombreIncident_, digits = 0, format ="f",big.mark=' ' ), 'Nombre d\'Incidents', color = "green")
                  
                })
                #-- Kpi Nombre de projets 
                output$NombreProjet<-renderValueBox({
                  dataf_Projet<-dataf_Projet()
                  NombreProjet_<-length(dataf_Projet$Projet)
                  valueBox(formatC(NombreProjet_, digits = 0, format ="f",big.mark=' ' ), 'Nombre de projets', color = "green")
                  
                })
                #-- Kpi Quantite de consommables 
                output$QuantiteConsommable<-renderValueBox({
                  dataf_Consommable<-dataf_Consommable_param()
                  QteConsommable_<-sum(dataf_Consommable$Nombre)
                  valueBox(formatC(QteConsommable_, digits = 0, format ="f",big.mark=' ' ), 'Quantite de consommables(U)', color = "green")
                  
                })
                
                
                #-- Kpi TauxRespectDelaiProjet 
                output$TauxRespectDelaiProjet<-renderValueBox({
                  dataf_Projet<-dataf_Projet_param()
                  TauxRespectDelai_<-mean(dataf_Projet$RespectDelai)
                  valueBox(formatC(TauxRespectDelai_, digits = 2, format ="f",big.mark=' ' ), 'Taux-respect-delai-projets(%)', color = "black")
                  
                })
                
                
                #-- Kpi TauxSatisfactionUtilisateur 
                output$TauxSatisfactionUtilisateur<-renderValueBox({
                  dataf_Satisfaction<-dataf_Satisfaction_param()
                  data_votant<-dataf_Satisfaction %>% filter(Votant != 0)
                  #Calcul de la somme des notes
                  NoteTotal <- sum(data_votant$Votant)
                  #Calcul du taux de satisfaction
                  TauxSatis <-  ((NoteTotal) / sum(data_votant$Poids)) * 100
                  
                  valueBox(formatC(TauxSatis, digits = 2, format ="f",big.mark=' ' ), 'Taux de satisfaction(%)', color = "black")
                  
                })
                
                
                #-- Kpi TauxParticipation 
                output$TauxParticipation<-renderValueBox({
                  dataf_Satisfaction<-dataf_Satisfaction_param()
                  data_votant<-dataf_Satisfaction %>% filter(Votant != 0)
                  #Calcul du taux de participation
                  TauxParticipation <-  (length(data_votant$Votant) / length(dataf_Satisfaction$Votant)) * 100
                  
                  valueBox(formatC(TauxParticipation, digits = 2, format ="f",big.mark=' ' ), 'Taux de participation(%)', color = "black")
                  
                })
                
                #---------------FIN Chargement des widgets INFORMATIQUE-----------------------
                
                
                #creating the valueBoxOutput content
                output$value1 <- renderValueBox({
                    dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                    #dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                    dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    
                    Valeur1 <- sum(dataf_productionUsine$PdsCgEgrene)/1000
                    value1G<<-Valeur1
                    valueBox(formatC(Valeur1,digits = 0,format ="f",big.mark=' ' ),'TonnageCgEgrene(t)',color = "green")
                    
                })
                
                output$value2 <- renderValueBox({
                    dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                    dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur2 <- sum(dataf_productionUsine$PdsFibreCourtes)/1000
                    valueBox(formatC(Valeur2,digits = 0,format ="f",big.mark=' ' ),'TonnageFibreCourte(t)',color = "green")
                })
                
                output$value3 <- renderValueBox({
                    dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                    dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur3a <- sum(dataf_productionUsine$PdsCgEgrene)
                    
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur3b <- sum(dataf_production$PoidsNet)
                    
                    Valeur3=(Valeur3b/Valeur3a)*100
                    
                    valueBox(formatC(Valeur3,digits = 2,format ="f",big.mark=' ' ),'Rendement fibre(%)',color = "black")
                })
                
                #creating the valueBoxOutput content
                output$value4 <- renderValueBox({
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$NbBalles),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur4 <- sum(dataf_production$NbBalles)
                    valueBox(formatC(Valeur4,digits = 0,format ="f",big.mark=' ' ),'Nombre de Balls',color = "green")
                })
                
                output$value5 <- renderValueBox({
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur5 <- sum(dataf_production$PoidsNet)/1000
                    valueBox(formatC(Valeur5,digits = 0,format ="f",big.mark=' ' ),'TonnageBalles Net (t)',color = "green")
                })
                
                output$value6 <- renderValueBox({
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    Valeur6 <- sum(dataf_production$PoidsNet)/sum(dataf_production$NbBalles)
                    valueBox(formatC(Valeur6,digits = 2,format ="f",big.mark=' ' ),'PoidsMoyen Balles (Kg)',color = "black")
                })
                
                output$value7 <- renderValueBox({
                  dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                  dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur7 <- sum(dataf_productionUsine$PdsCgEgrene)/nrow(dataf_productionUsine)/1000
                  valueBox(formatC(Valeur7,digits = 0,format ="f",big.mark=' ' ),'Egrené moyen par jour(t)',color = "black")
                })
                
                output$value8 <- renderValueBox({
                  dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur8<- sum(dataf_production$KwhReseau)/(sum(dataf_production$PoidsNet)/1000)
                  valueBox(formatC(Valeur8,digits = 0,format ="f",big.mark=' ' ),'Kwh Conso./t de fibre',color = "black")
                })
                
                output$value9 <- renderValueBox({
                  dataf_production<-dataf_production()
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur9 <- sum(dataf_production$TempsEgreneuse1+dataf_production$TempsEgreneuse2+dataf_production$TempsEgreneuse3+dataf_production$TempsEgreneuse4)
                  valueBox(formatC(Valeur9,digits = 0,format ="f",big.mark=' ' ),'Fonct.Egreneuse (H)',color = "black")
                })
                output$value10 <- renderValueBox({
                  dataf_production<-dataf_production()[complete.cases(dataf_production()$NbEgreneuses),]
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  #Valeur10 <- sum(dataf_production$NbScies)/nrow(dataf_production)
                  Valeur10 <- max(dataf_production$NbScies)
                  valueBox(formatC(Valeur10,digits = 0,format ="f",big.mark=' ' ),'Nbre de scies',color = "purple")
                })
                
                
                output$value11 <- renderValueBox({
                  #dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                  #dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                  #Valeur11a <- sum(dataf_productionUsine$PdsFibreCourtes)
                  
                  dataf_production<-dataf_production()[complete.cases(dataf_production()$NbEgreneuses),]
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur11a <- sum(dataf_production$PoidsNet)
                  Valeur11b <- max(dataf_production$NbScies)
                  
                  Valeur11c <- sum(dataf_production$TempsEgreneuse1+dataf_production$TempsEgreneuse2+dataf_production$TempsEgreneuse3+dataf_production$TempsEgreneuse4)
                  
                  Valeur11 = Valeur11a/(Valeur11b*Valeur11c)
                  valueBox(formatC(Valeur11,digits = 2,format ="f",big.mark=' ' ),'Vitesse egrenage (Ksh)',color = "black")
                })
                
                output$value12 <- renderValueBox({
                  dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                  dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur12 <- nrow(dataf_productionUsine)
                  valueBox(formatC(Valeur12,digits = 0,format ="f",big.mark=' ' ),'Nbre de Jours Egrenage',color = "green")
                })
                
                output$TabBallesCompare = renderDT({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$NbBalles),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine)
                    
                    dataf_production<-dataf_production%>%
                        group_by(Equipe) %>%
                        summarize(
                                  #NbBalles_Mois= sum((NbBalles[Month==VMonth])),
                                  PoidsMoy_Mois= sum((PoidsNet[Month==input$Month]))/sum((NbBalles[Month==input$Month])),
                                  #NbBalles_MoisPrec= sum((NbBalles[Month== VMonthPrec])),
                                  PoidsMoy_MoisPrec= sum((PoidsNet[Month==as.character(as.numeric(input$Month)-1)] ))/sum((NbBalles[Month==as.character(as.numeric(input$Month)-1)]))
                                  )
                    dataf_production$variation_PoidsMoy<-dataf_production$PoidsMoy_Mois-dataf_production$PoidsMoy_MoisPrec
                    
                    dataf_production$PoidsMoy_Mois<-formatC(dataf_production$PoidsMoy_Mois,digits = 1,format ="f",big.mark=',' )
                    dataf_production$PoidsMoy_MoisPrec<-formatC(dataf_production$PoidsMoy_MoisPrec,digits = 1,format ="f",big.mark=',' )
                    dataf_production$variation_PoidsMoy<-formatC(dataf_production$variation_PoidsMoy,digits = 1,format ="f",big.mark=',' )
                    datatable(dataf_production, rownames = FALSE) 
                })
                
                
                output$GNbBalles <- renderPlotly({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$Ecart_Poids),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    
                    if(nrow(dataf_production)>0){
                        p<-ggplot(dataf_production,aes(x=DateOpe))+
                            geom_line(aes(y=NbBalles,col="NbBalles"), 
                                      alpha = 0.6,
                                      size = 0.9) +
                            labs(x = "Date", 
                                 y = "Nombre de Balles",
                                 title = "Evolution Journaliere Nombre de balles")+ 
                            scale_color_manual(values = c(
                                'NbBalles' = 'green'
                            )
                            )+
                            labs(color = '')+
                            theme_minimal()
                        ggplotly(p)
                    }
                }
                )
                
                output$GNbBallesQuart <- renderPlotly({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$Ecart_Poids),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    
                    if(nrow(dataf_production)>0){
                      p<-ggplot(dataf_production, aes(x = DateOpe, y = NbBalles)) + 
                            geom_line(aes(color = Quart, linetype = Quart),
                                      #alpha = 0.6,
                                      size = 0.5       
                                      ) + 
                            labs(x = "Date", 
                                 y = "Nombre de Balles",
                                 title = "Nombre de balles par Quart")+ 
                            
                            #scale_color_manual(values = c("darkred", "steelblue","blue"))+
                            labs(color = '')+
                            theme_minimal()
                        ggplotly(p)
                    }
                }
                )
                
                output$GNbBallesEquipe <- renderPlotly({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$Ecart_Poids),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    
                    if(nrow(dataf_production)>0){
                        p<-ggplot(dataf_production, aes(x = DateOpe, y = NbBalles)) + 
                            geom_line(aes(color = Equipe, linetype = Equipe),
                                      #alpha = 0.6,
                                      size = 0.5       
                            ) + 
                            labs(x = "Date", 
                                 y = "Nombre de Balles",
                                 title = "Nombre de balles par Equipe")+ 
                            
                            #scale_color_manual(values = c("darkred", "steelblue","blue"))+
                            labs(color = '')+
                            theme_minimal()
                        ggplotly(p)
                    }
                }
                )
                
               
                output$GNbBallesEquipes <- renderPlotly({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$Ecart_Poids),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    dataf_production<-dataf_production %>%
                        group_by(Equipe,Quart) %>%
                        summarize(TotalBalles=sum(NbBalles))
                    
                    if(nrow(dataf_production)>0){
                        p<-ggplot(data=dataf_production, aes(x = Equipe, y = TotalBalles, fill = Quart)) + 
                            geom_bar(stat = "identity")+ 
                            labs(x = "Equipe", 
                                 y = "Nombre de Balles",
                                 title = "Nombre de balles par Equipe et par Quart")+ 
                            theme_minimal()
                        ggplotly(p)
                    }
                }
                )
                
                
                output$TabBalles = renderDT({
                    dataf_production<-dataf_production()
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$Ecart_Poids),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    dataf_production<-head(dataf_production[order(-dataf_production$NbBalles),],5)
                    dataf_production<-dataf_production[,c("DateOpe","DayOfWeek","Equipe","Quart","NbBalles")]
                    datatable(dataf_production, rownames = FALSE) 
                })
                
                
                output$TabRendement = renderDT({
                    dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                    #dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                    dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    dataf_productionUsine<-dataf_productionUsine[,c("DateOpe","PdsCgEgrene","PdsFibreCourtes","PdsGraines")]
                    dataf_productionUsine$DateOpe<-format(dataf_productionUsine$DateOpe,"%b %d %Y")
                    dataf_productionUsine$PoidsNet<-0
                    
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    #dataf_production<-dataf_production %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                    dataf_production<-dataf_production[,c("DateOpe","PoidsNet")]
                    dataf_production$DateOpe<-format(dataf_production$DateOpe,"%b %d %Y")
                    dataf_production$PdsCgEgrene<-0
                    dataf_production$PdsFibreCourtes<-0
                    dataf_production$PdsGraines<-0
                    
                    dataf_production<-rbind(dataf_production,dataf_productionUsine)
                    
                    dataf_production<-dataf_production %>%
                      group_by(DateOpe) %>%
                      summarize(PdsCgEgrene=sum(PdsCgEgrene),TonnageFibre=sum(PoidsNet),PdsGraines=sum(PdsGraines),
                                PdsFibreCourtes=sum(PdsFibreCourtes),
                                RendementFibre=100*sum(PoidsNet)/sum(PdsCgEgrene)
                      )
                    
                    #dataf_productionUsine$RendementFibre<-100*dataf_productionUsine$PdsFibreCourtes/dataf_productionUsine$PdsCgEgrene
                    dataf_production$RendementFibre<-formatC(dataf_production$RendementFibre,digits = 2,format ="f",big.mark=',',width = 2)
                    
                    
                    
                    names(dataf_production)<-c("DateOpe","CgEgrene(kg)","NetFibre(Kg)","Graines(Kg)", "Dechet(kg)", "%Fibre")
                    
                    
                    datatable(dataf_production, rownames = FALSE,selection = 'single',
                              options=list(
                                  pageLength = 5,
                                  lengthMenu = c(5),
                                  autoWidth = FALSE
                              ))
                })
                
  
                output$GRendementFibre <- renderPlotly({
                    dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                    dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    dataf_productionUsine<-dataf_productionUsine %>%
                        group_by(DateOpe) %>%
                        summarize(PdsCgEgrene=sum(PdsCgEgrene),PoidsNet=sum(0)
                                  )
                    
                    dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                    dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                    dataf_production<-dataf_production %>%
                      group_by(DateOpe) %>%
                      summarize(PdsCgEgrene=sum(0),PoidsNet=sum(PoidsNet)
                      )
                    
                    dataf_productionUsine<-rbind(dataf_productionUsine,dataf_production)
                    
                    dataf_productionUsine<-dataf_productionUsine %>%
                      bind_rows(.id = "location") %>% 
                      group_by(DateOpe) %>%
                      summarize(TonnageFibre=100*sum(PoidsNet)/sum(PdsCgEgrene)
                      )
                    
                    dataf_productionUsine['Objectif'] = 43
                    
                    if(nrow(dataf_productionUsine)>0){
                      p<-ggplot(data=dataf_productionUsine) + 
                        geom_line(aes(x=DateOpe,y=TonnageFibre),color='red') + 
                        geom_line(aes(x=DateOpe,y=Objectif),color='blue') + 
                        ylab('Values')+xlab('date')
                      
                        #p<-ggplot(data=dataf_productionUsine, aes(x = DateOpe, y = TonnageFibre)) + 
                        #    geom_line(size = 0.5       
                        #    ) + 
                        #    labs(x = "DateProduction", 
                        #         y = "Evolution Rendement fibre(%))",
                        #         title = "Rendement Fibre(%)")+  
                        #    theme_minimal()
                        #ggplotly(p)
                    }
                }
                )
                
                
    
    #----------------- CLASSEMENT --------------------------------------------------------------
                
                
                output$TabclassementJour = renderDT({
                  
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement$TonnageFibre <- dataf_classement$PoidsNet/1000
                  dataf_classement$NbBalles<-1
                  
                  dataf_classement$TonFibre_HautGamme<- ifelse(dataf_classement$Qualite=="Haut de gamme",dataf_classement$PoidsNet/1000,0)
                  dataf_classement$TonFibre_Reference<- ifelse(dataf_classement$Qualite=="Fibre de référence",dataf_classement$PoidsNet/1000,0)
                  dataf_classement$TonFibre_MoyGamme<- ifelse(dataf_classement$Qualite=="Moyenne gamme",dataf_classement$PoidsNet/1000,0)
                  dataf_classement$TonFibre_BasGamme<- ifelse(dataf_classement$Qualite=="Bas gamme",dataf_classement$PoidsNet/1000,0)
                  dataf_classement$NbBalle_Avarie<- ifelse(!is.na(dataf_classement$IdAvaries),1,0)
                  dataf_classement$Tonnage_Avarie<- ifelse(!is.na(dataf_classement$IdAvaries),dataf_classement$PoidsNet/1000,0)
                  
                  
                  dataf_classement$DateOpe<-format(dataf_classement$DateOpe,"%b %d %Y")
                  
                  '
                  Valeur14 <- nrow(dataf_classement)
                  
                  dataf_productionUsine<-dataf_productionUsine()[complete.cases(dataf_productionUsine()$PdsCgEgrene),]
                  #dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                  dataf_productionUsine<-dataf_productionUsine %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  dataf_productionUsine<-dataf_productionUsine[,c("DateOpe","PdsCgEgrene","PdsFibreCourtes","PdsGraines")]
                  dataf_productionUsine$DateOpe<-format(dataf_productionUsine$DateOpe,"%b %d %Y")
                  dataf_productionUsine$PoidsNet<-0
                  
                  dataf_production<-dataf_production()[complete.cases(dataf_production()$PoidsNet),]
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  #dataf_production<-dataf_production %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                  dataf_production<-dataf_production[,c("DateOpe","PoidsNet")]
                  dataf_production$DateOpe<-format(dataf_production$DateOpe,"%b %d %Y")
                  dataf_production$PdsCgEgrene<-0
                  dataf_production$PdsFibreCourtes<-0
                  dataf_production$PdsGraines<-0
                  
                  dataf_production<-rbind(dataf_production,dataf_productionUsine)
                  '
                  dataf_classement<-dataf_classement %>%
                    group_by(DateOpe) %>%
                    summarize(TonnageFibre=sum(TonnageFibre),
                              NbBalles=sum(NbBalles),
                              TonFibre_HautGamme=sum(TonFibre_HautGamme),
                              TonFibre_Reference=sum(TonFibre_Reference),
                              TonFibre_MoyGamme=sum(TonFibre_MoyGamme),
                              TonFibre_BasGamme=sum(TonFibre_BasGamme),
                              NbBalle_Avarie=sum(NbBalle_Avarie),
                              Tonnage_Avarie=sum(Tonnage_Avarie)
                              
                    )
                  
                  dataf_classement$TonnageFibre<-formatC(dataf_classement$TonnageFibre,digits = 2,format ="f",big.mark=',',width = 2)
                  dataf_classement$TonFibre_Reference<-formatC(dataf_classement$TonFibre_Reference,digits = 2,format ="f",big.mark=',',width = 2)
                  dataf_classement$TonFibre_HautGamme<-formatC(dataf_classement$TonFibre_HautGamme,digits = 2,format ="f",big.mark=',',width = 2)
                  dataf_classement$TonFibre_MoyGamme<-formatC(dataf_classement$TonFibre_MoyGamme,digits = 2,format ="f",big.mark=',',width = 2)
                  dataf_classement$TonFibre_BasGamme<-formatC(dataf_classement$TonFibre_BasGamme,digits = 2,format ="f",big.mark=',',width = 2)
                  dataf_classement$Tonnage_Avarie<-formatC(dataf_classement$Tonnage_Avarie,digits = 2,format ="f",big.mark=',',width = 2)
                  
                  
                  #names(dataf_production)<-c("DateOpe","CgEgrene(kg)","NetFibre(Kg)","Graines(Kg)", "Dechet(kg)", "%Fibre")
                  datatable(dataf_classement, rownames = FALSE,selection = 'single',
                            options=list(
                              pageLength = 5,
                              lengthMenu = c(5),
                              autoWidth = FALSE
                            ))
                })
                
                
                
                
                output$value13 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  Valeur13 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur13,digits = 0,format ="f",big.mark=' ' ),'TonnageFibre(t)',color = "green")
                })
                output$value14 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  Valeur14 <- nrow(dataf_classement)
                  valueBox(formatC(Valeur14,digits = 0,format ="f",big.mark=' ' ),'Nombre de balles',color = "green")
                })
                output$value15 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Haut de gamme')
                  Valeur15 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur15,digits = 0,format ="f",big.mark=' ' ),'Ton. fibre Haut gamme(t)',color = "green")
                })
                output$value16 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Fibre de référence')
                  Valeur16 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur16,digits = 0,format ="f",big.mark=' ' ),'Ton. fibre Référence(t)',color = "green")
                })
                output$value17 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Moyenne gamme')
                  Valeur17 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur17,digits = 0,format ="f",big.mark=' ' ),'Ton. Moyenne gamme(t)',color = "green")
                })
                output$value18 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Bas gamme')
                  Valeur18 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur18,digits = 0,format ="f",big.mark=' ' ),'Ton. Bas gamme(t)',color = "green")
                })
                
                output$value19 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$IdAvaries),]
                  Valeur19 <- sum(dataf_classement$PoidsNet)/1000
                  valueBox(formatC(Valeur19,digits = 0,format ="f",big.mark=' ' ),'Tonnage AVARIES(t)',color = "black")
                })
                output$value20 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$IdAvaries),]
                  Valeur20 <- nrow(dataf_classement)
                  valueBox(formatC(Valeur20,digits = 0,format ="f",big.mark=' ' ),'Nombre de balles AVARIES',color = "black")
                })
                
                output$value21 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$IdAvaries),]
                  Valeur21a <- nrow(dataf_classement)
                  
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  Valeur21b <- nrow(dataf_classement)
                  
                  Valeur21<-(Valeur21a/Valeur21b)*100
                  valueBox(formatC(Valeur21,digits = 3,format ="f",big.mark='.' ),'Taux de balles AVARIES(%)',color = "red")
                })
                
                output$TabtauxFibre = renderDT({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Classe) %>%
                    summarize(#TonBrut=sum(PoidsBrut)/1000,
                      TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  dataf_classement$TauxFibre<-(dataf_classement$TonNet/VTotalFibre)*100
                  dataf_classement$TauxFibre <-formatC(dataf_classement$TauxFibre,digits = 3,format ="f",big.mark=' ' )
                  
                  #dataf_classement$TonBrut <-formatC(dataf_classement$TonBrut,digits = 2,format ="f",big.mark=' ' )
                  dataf_classement$TonNet <-formatC(dataf_classement$TonNet,digits = 2,format ="f",big.mark=' ' )
                  names(dataf_classement)<-c("Classe","TonnageNet(t)","Taux(%)")
                  datatable(dataf_classement, rownames = FALSE,
                            options=list(
                              pageLength = 5,
                              lengthMenu = c(5), 
                              autoWidth = FALSE)
                            ) 
                })  
                
                output$LongueurSoie = renderDT({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Soie) %>%
                    summarize(#TonBrut=sum(PoidsBrut)/1000,
                              TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  
                  #dataf_classement$Totol<- sum(dataf_classement$PoidsNet)
                  dataf_classement$Taux<-(dataf_classement$TonNet/VTotalFibre)*100
                  dataf_classement$Taux <-formatC(dataf_classement$Taux,digits = 3,format ="f",big.mark=',' )
                  
                  #dataf_classement$TonBrut <-formatC(dataf_classement$TonBrut,digits = 2,format ="f",big.mark=' ' )
                  dataf_classement$TonNet <-formatC(dataf_classement$TonNet,digits = 2,format ="f",big.mark=' ' )
                  names(dataf_classement)<-c("Soie","TonnageNet","Taux(%)")
                  datatable(dataf_classement, rownames = FALSE) 
                })
                
                output$TabtauxFibreSoie = renderDT({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Variete) %>%
                    summarize(TonBrut=sum(PoidsBrut)/1000,TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  dataf_classement$TauxFibre<-(dataf_classement$TonNet/VTotalFibre)*100
                  dataf_classement$TauxFibre <-formatC(dataf_classement$TauxFibre,digits = 3,format ="f",big.mark=' ' )
                  
                  dataf_classement$TonBrut <-formatC(dataf_classement$TonBrut,digits = 2,format ="f",big.mark=' ' )
                  dataf_classement$TonNet <-formatC(dataf_classement$TonNet,digits = 2,format ="f",big.mark=' ' )
                  datatable(dataf_classement, rownames = FALSE) 
                })
                
                output$GTauxFibreCategorie <- renderPlotly({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Classe) %>%
                    summarize(TonBrut=sum(PoidsBrut)/1000,TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  dataf_classement$TauxFibre<-(dataf_classement$TonNet/VTotalFibre)*100
                  
                   dataf_classement<-arrange(dataf_classement,-TauxFibre) 
                  dataf_classement = dataf_classement %>% mutate_if(is.factor,
                                                                    fct_explicit_na,
                                                                    na_level = "NA")
                  
                  dataf_classement$Classe  <- with(dataf_classement, reorder(Classe, -TauxFibre))
                  p<- ggplot(data=dataf_classement, aes(x = reorder(Classe, TauxFibre), y = TauxFibre, fill = Classe)) + 
                    geom_bar(stat = "identity")+ 
                    coord_flip()+
                    labs(x = "Classe", 
                         y = "Taux(%)"
                         )+
                    theme(axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
                  #theme_minimal()
                  ggplotly(p)
                  
                }
                )
                
                output$GTauxFibreCategorie2 <- renderPlotly({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  dataf_classement<-dataf_classement %>%
                    group_by(DateOpe,Qualite) %>%
                    summarize(TonNet=sum(PoidsNet)/1000
                    )
                  #VTotalFibre <-sum(dataf_classement$TonNet)
                  #dataf_classement$TauxFibre<-dataf_classement$TonNet/VTotalFibre
                  
                  if(nrow(dataf_classement)>0){
                    p<-ggplot(data=dataf_classement) + 
                      geom_line(aes(x=DateOpe,y=TonNet,color=Qualite)) + 
                      #geom_line(aes(x=DateOpe,y=Objectif),color='blue') + 
                      ylab('Tonnage(t)')+xlab('date')
                    }
                  
                  ggplotly(p)
                  
                }
                )
                
                
                output$TabtauxFibreVariete = renderDT({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Variete) %>%
                    summarize(#TonBrut=sum(PoidsBrut)/1000,
                              TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  dataf_classement$TauxFibre<-(dataf_classement$TonNet/VTotalFibre)*100
                  dataf_classement$TauxFibre <-formatC(dataf_classement$TauxFibre,digits = 3,format ="f",big.mark=' ' )
                  
                  #dataf_classement$TonBrut <-formatC(dataf_classement$TonBrut,digits = 2,format ="f",big.mark=' ' )
                  dataf_classement$TonNet <-formatC(dataf_classement$TonNet,digits = 2,format ="f",big.mark=' ' )
                  names(dataf_classement)<-c("Variete","TonnageNet","Taux(%)")
                  datatable(dataf_classement, rownames = FALSE) 
                })
                
                output$GTauxFibreSoie <- renderPlotly({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  #dataf_classement<-dataf_classement %>% filter(Periode == input$Year & IdUsine==input$Usine & as.numeric(Month)==as.numeric(input$Month))
                  dataf_classement<-dataf_classement %>%
                    group_by(Soie) %>%
                    summarize(TonBrut=sum(PoidsBrut)/1000,TonNet=sum(PoidsNet)/1000
                    )
                  VTotalFibre <-sum(dataf_classement$TonNet)
                  dataf_classement$TauxFibre<-(dataf_classement$TonNet/VTotalFibre)*100
                  
                  ggplot(dataf_classement, aes(x = 3, y = TauxFibre, fill = Soie)) +
                    geom_bar(stat = "identity", color = "white") +
                    #labs(title  = "Ventes par Type de Client")+
                    #coord_polar(theta = "y", start = 0)+
                    geom_text(aes(y = TauxFibre, label = paste0(TauxFibre,
                                                               " (",
                                                               scales::percent(TauxFibre / sum(TauxFibre)),
                                                               ")")), 
                              color = "black",
                              #position = position_dodge(width = 1),
                              #vjust = -2, 
                              #size = 5
                              )+
                    
                    #scale_fill_manual(values = mycols) +
                    theme_void()+
                    theme(
                      plot.title = element_text(color="black", size=16, face="bold.italic"))#+
                    #xlim(0.5, 2.5)
                  
                }
                )
                
        #----------------- MAINTENANCE --------------------------------------------------------------
                output$value71 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Valeur71 <- sum(dataf_Panne$NbPanne)
                  valueBox(formatC(Valeur71,digits = 0,format ="f",big.mark=' ' ),'Nbre de pannes du Mois',color = "black")
                })
                
                output$value72 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Valeur72 <- sum(dataf_Panne$DureePanne)/60
                  valueBox(formatC(Valeur72,digits = 0,format ="f",big.mark=' ' ),'Duree de pannes du Mois (Heures) ',color = "yellow")
                })
                
                output$value73 <- renderValueBox({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  Valeur73 <- sum(dataf_PanneCumul$NbPanne)
                  valueBox(formatC(Valeur73,digits = 0,format ="f",big.mark=' ' ),'Cumul annuel Nbre de pannes',color = "black")
                })
                
                output$value74 <- renderValueBox({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  Valeur74 <- sum(dataf_PanneCumul$DureePanne)/60
                  valueBox(formatC(Valeur74,digits = 0,format ="f",big.mark=' ' ),'Cumul annuel Duree pannes (Heures) ',color = "yellow")
                })
                
                
                output$value100 <- renderValueBox({
                  dataf_production<-dataf_production()
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine & Month==input$Month)
                  Valeur100 <- sum(dataf_production$TempsCondenseur)
                  valueBox(formatC(Valeur100,digits = 2,format ="f",big.mark=' ' ),'TempsCondenseur Mois (H)',color = "black")
                })
                output$value101 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Valeur101 <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60
                  valueBox(formatC(Valeur101,digits = 2,format ="f",big.mark=' ' ),'Heure Cadence Mois (H)',color = "yellow")
                })
                
                output$value102 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur102a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur102b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur102c<-Valeur102a-Valeur102b #temps de fonctionnement
                  Valeur102d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur102<-Valeur102c-Valeur102d #Temps Net
                  valueBox(formatC(Valeur102,digits = 2,format ="f",big.mark=' ' ),'Temps Net Mois (H)',color = "green")
                })
                
                
                output$TabTempsNetEvolution = renderDT({
                  dataf_production<-dataf_production()
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine #& Month<=input$Month
                                                                )
                  dataf_production<-dataf_production %>%
                    group_by(MonthName,Month) %>%
                    summarize(Temps=sum(TempsCondenseur)
                    )
                  
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul<-dataf_PanneCumul[dataf_PanneCumul$clasification_machi=="cadence",]
                  dataf_PanneCumul<-dataf_PanneCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(Temps=-sum(DureePanne)/60
                    )
                  
                  dataf_production<-rbind(dataf_production,dataf_PanneCumul)
                  
                  dataf_production<-dataf_production %>%
                    group_by(MonthName,Month) %>%
                    summarize(TempsNet=sum(Temps)
                    )
                  
                  names(dataf_production)<-c("Mois","Month","TempsNet")
                  dataf_production <- dataf_production[order(dataf_production$Month),c("Mois","TempsNet")]
                  dataf_production$TempsNet<- formatC(dataf_production$TempsNet,digits = 2,format ="f",big.mark=' ' )
                  names(dataf_production)<-c("Mois","TempsNet(H)")
                  
                  datatable(dataf_production, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                
                output$GEvolutionTempsNetParMois <- renderPlotly({
                  dataf_production<-dataf_production()
                  dataf_production<-dataf_production %>% filter(Periode == input$Year & IdUsine==input$Usine #& Month<=input$Month
                                                                )
                  dataf_production<-dataf_production %>%
                    group_by(MonthName,Month) %>%
                    summarize(Temps=sum(TempsCondenseur)
                    )
                  
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul<-dataf_PanneCumul[dataf_PanneCumul$clasification_machi=="cadence",]
                  dataf_PanneCumul<-dataf_PanneCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(Temps=-sum(DureePanne)/60
                    )
                  
                  dataf_production<-rbind(dataf_production,dataf_PanneCumul)
                  
                  dataf_production<-dataf_production %>%
                    group_by(MonthName,Month) %>%
                    summarize(TempsNet=sum(Temps)
                    )
                  
                  names(dataf_production)<-c("Mois","Month","TempsNet")
                  #dataf_production <- dataf_production[order(dataf_production$Month),c("Mois","TempsNet")]
                  #dataf_production$TempsNet<- formatC(dataf_production$TempsNet,digits = 2,format ="f",big.mark=' ' )
                  #names(dataf_production)<-c("Mois","TempsNet(H)")
                  
                  #p<-ggplot(data=dataf_production, aes(x = Equipe, y = TotalBalles, fill = Quart)) + 
                  #  geom_bar(stat = "identity")+ 
                  #  labs(x = "Equipe", 
                  #       y = "Nombre de Balles",
                  #       title = "Nombre de balles par Equipe et par Quart")+ 
                  #  theme_minimal()
                  #ggplotly(p)
                  
                  
                  p<- ggplot(data=dataf_production, aes(x = reorder(Mois, Month), y = TempsNet, fill = Mois)) + 
                    geom_bar(stat = "identity")+ 
                    #coord_flip()+
                    labs(x = "Mois", 
                         y = "TempsNet"
                    )#+
                    #theme(axis.title.y=element_blank(),
                    #      axis.text.y=element_blank(),
                    #      axis.ticks.y=element_blank())
                  #theme_minimal()
                  ggplotly(p)
                  
                  
                }
                )
                
                
                output$value103 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  value103<-100*(days_in_month(Vdate)*24-(sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60))/(days_in_month(Vdate)*24)
                  valueBox(formatC(value103,digits = 2,format ="f",big.mark=' ' ),'Taux rendement Economique(%)',color = "blue")
                })
                
                output$value104 <- renderValueBox({
                  
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur104a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur104b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur104c<-Valeur104a-Valeur104b #temps de fonctionnement
                  
                  Valeur104<-(Valeur104c/Valeur104a)*100 #Taux de disponibilite operationnelle
                  valueBox(formatC(Valeur104,digits = 2,format ="f",big.mark=' ' ),'Taux de dispo Operationnelle(%)',color = "blue")
                })
                
                output$value105 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur105a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur105b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur105c<-Valeur105a-Valeur105b #temps de fonctionnement
                  Valeur105d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur105e<-Valeur105c-Valeur105d #Temps Net
                  
                  value105<- (Valeur105e/Valeur105c)*100
                  valueBox(formatC(value105,digits = 2,format ="f",big.mark=' ' ),'Taux de performance(%)',color = "blue")
                })
                
                
                output$value106 <- renderValueBox({
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  Valeur106a <- sum(dataf_classement$PoidsNet)/1000 # tonnage fibre
                  
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Haut de gamme')
                  Valeur106b <- sum(dataf_classement$PoidsNet)/1000 # Tonnage fibre haut de gamme
                  Valeur106<- (Valeur106b/Valeur106a)*100
                  valueBox(formatC(Valeur106,digits = 2,format ="f",big.mark=' ' ),'Taux de Qualité (%)',color = "blue")
                  
                
                  })
                
                output$value107 <- renderValueBox({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur104a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur104b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur104c<-Valeur104a-Valeur104b #temps de fonctionnement
                  Valeur104<-(Valeur104c/Valeur104a) #Taux de disponibilite operationnelle
                  
                  
                  Valeur105a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur105b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur105c<-Valeur105a-Valeur105b #temps de fonctionnement
                  Valeur105d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur105e<-Valeur105c-Valeur105d #Temps Net
                  valeur105<- (Valeur105e/Valeur105c) #Taux de Performance
                  
                  
                  dataf_classement<-dataf_classement()
                  dataf_classement<-dataf_classement()[complete.cases(dataf_classement()$PoidsNet),]
                  Valeur106a <- sum(dataf_classement$PoidsNet)/1000 # tonnage fibre
                  dataf_classement<-dataf_classement %>% filter(Qualite == 'Haut de gamme')
                  Valeur106b <- sum(dataf_classement$PoidsNet)/1000 # Tonnage fibre haut de gamme
                  Valeur106<- (Valeur106b/Valeur106a) #TAux de Qualite
                  
                  Valeur107<-Valeur104*valeur105*Valeur106 *100
                  
                  valueBox(formatC(Valeur107,digits = 2,format ="f",big.mark=' ' ),'taux rendement synthetique (%)',color = "blue")
                  
                  
                })
                
                
                output$TabTemps = renderDT({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur102a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur102b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur102b1 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene"),]$DureePanne)/60 #heure Exogene
                  Valeur102b2 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("machine"),]$DureePanne)/60 #heure Machine
                  Valeur102b3 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("fabrication"),]$DureePanne)/60 #heure Fabrication
                  
                  Valeur102c<-Valeur102a-Valeur102b #temps de fonctionnement
                  Valeur102d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur102<-Valeur102c-Valeur102d #Temps Net
                  Valeur102e<-days_in_month(Vdate)*24#Temps Total
                  Valeur102f <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("maintenance"),]$DureePanne)/60 #heure Maintenance
                  Valeur102g <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("chomer"),]$DureePanne)/60 #heure Chomer
                  Valeur102h<-Valeur102e-Valeur102g #Temps Ouverture
                  
                  TempsOuverture<-data.frame("Temps Ouverture",Valeur102h)
                  names(TempsOuverture)<-c("Rubrique","Temps(H)")
                  TempsChomer<-data.frame("Temps Chomé",Valeur102g)
                  names(TempsChomer)<-c("Rubrique","Temps(H)")
                  TempsMaintenance<-data.frame("Temps Maintenance",Valeur102f)
                  names(TempsMaintenance)<-c("Rubrique","Temps(H)")
                  TempsTotal<-data.frame("Temps Total",Valeur102e)
                  names(TempsTotal)<-c("Rubrique","Temps(H)")
                  TempsCadence<-data.frame("Temps Cadences",Valeur102d)
                  names(TempsCadence)<-c("Rubrique","Temps(H)")
                  TempsRequis<-data.frame("Temps Requis",Valeur102a)
                  names(TempsRequis)<-c("Rubrique","Temps(H)")
                  TempsExogen<-data.frame("Temps Exogene",Valeur102b1)
                  names(TempsExogen)<-c("Rubrique","Temps(H)")
                  TempsMachine<-data.frame("Temps Machine",Valeur102b2)
                  names(TempsMachine)<-c("Rubrique","Temps(H)")
                  TempsFabrication<-data.frame("Temps Fabrication",Valeur102b3)
                  names(TempsFabrication)<-c("Rubrique","Temps(H)")
                  TempsFonctionnement<-data.frame("Temps Fonctionnement",Valeur102c)
                  names(TempsFonctionnement)<-c("Rubrique","Temps(H)")
                  TempsNet<-data.frame("Temps Net",Valeur102)
                  names(TempsNet)<-c("Rubrique","Temps(H)")
                  
                  dataf_Temps<-rbind(TempsChomer,TempsFabrication,TempsCadence,TempsMaintenance,TempsExogen,TempsMachine,
                                     TempsTotal,TempsOuverture,TempsRequis,TempsFonctionnement,TempsNet
                                        )
                  dataf_Temps$"Temps(H)"<-formatC(dataf_Temps$"Temps(H)",digits = 2,format ="f",big.mark=' ' )
                 
                  datatable(dataf_Temps, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 11,
                              lengthMenu = c(11),
                              autoWidth = FALSE
                            ))
                  
                })
                
                
                output$TabTemps2 = renderDT({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur102a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur102b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur102b1 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene"),]$DureePanne)/60 #heure Exogene
                  Valeur102b2 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("machine"),]$DureePanne)/60 #heure Machine
                  Valeur102b3 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("fabrication"),]$DureePanne)/60 #heure Fabrication
                  
                  Valeur102c<-Valeur102a-Valeur102b #temps de fonctionnement
                  Valeur102d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur102<-Valeur102c-Valeur102d #Temps Net
                  Valeur102e<-days_in_month(Vdate)*24#Temps Total
                  Valeur102f <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("maintenance"),]$DureePanne)/60 #heure Maintenance
                  Valeur102g <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("chomer"),]$DureePanne)/60 #heure Chomer
                  Valeur102h<-Valeur102e-Valeur102g #Temps Ouverture
                  
                  TempsOuverture<-data.frame("Temps Ouverture",Valeur102h)
                  names(TempsOuverture)<-c("Rubrique","Temps(H)")
                  TempsChomer<-data.frame("Temps Chomé",Valeur102g)
                  names(TempsChomer)<-c("Rubrique","Temps(H)")
                  TempsMaintenance<-data.frame("Temps Maintenance",Valeur102f)
                  names(TempsMaintenance)<-c("Rubrique","Temps(H)")
                  TempsTotal<-data.frame("Temps Total",Valeur102e)
                  names(TempsTotal)<-c("Rubrique","Temps(H)")
                  TempsCadence<-data.frame("Temps Cadences",Valeur102d)
                  names(TempsCadence)<-c("Rubrique","Temps(H)")
                  TempsRequis<-data.frame("Temps Requis",Valeur102a)
                  names(TempsRequis)<-c("Rubrique","Temps(H)")
                  TempsExogen<-data.frame("Temps Exogene",Valeur102b1)
                  names(TempsExogen)<-c("Rubrique","Temps(H)")
                  TempsMachine<-data.frame("Temps Machine",Valeur102b2)
                  names(TempsMachine)<-c("Rubrique","Temps(H)")
                  TempsFabrication<-data.frame("Temps Fabrication",Valeur102b3)
                  names(TempsFabrication)<-c("Rubrique","Temps(H)")
                  TempsFonctionnement<-data.frame("Temps Fonctionnement",Valeur102c)
                  names(TempsFonctionnement)<-c("Rubrique","Temps(H)")
                  TempsNet<-data.frame("Temps Net",Valeur102)
                  names(TempsNet)<-c("Rubrique","Temps(H)")
                  
                  dataf_Temps<-rbind(TempsChomer,TempsFabrication,TempsCadence,TempsMaintenance
                  )
                  dataf_Temps$"Temps(H)"<-formatC(dataf_Temps$"Temps(H)",digits = 2,format ="f",big.mark=' ' )
                  
                  datatable(dataf_Temps, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 11,
                              lengthMenu = c(11),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabTemps3 = renderDT({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur102a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur102b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur102b1 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene"),]$DureePanne)/60 #heure Exogene
                  Valeur102b2 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("machine"),]$DureePanne)/60 #heure Machine
                  Valeur102b3 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("fabrication"),]$DureePanne)/60 #heure Fabrication
                  
                  Valeur102c<-Valeur102a-Valeur102b #temps de fonctionnement
                  Valeur102d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur102<-Valeur102c-Valeur102d #Temps Net
                  Valeur102e<-days_in_month(Vdate)*24#Temps Total
                  Valeur102f <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("maintenance"),]$DureePanne)/60 #heure Maintenance
                  Valeur102g <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("chomer"),]$DureePanne)/60 #heure Chomer
                  Valeur102h<-Valeur102e-Valeur102g #Temps Ouverture
                  
                  TempsOuverture<-data.frame("Temps Ouverture",Valeur102h)
                  names(TempsOuverture)<-c("Rubrique","Temps(H)")
                  TempsChomer<-data.frame("Temps Chomé",Valeur102g)
                  names(TempsChomer)<-c("Rubrique","Temps(H)")
                  TempsMaintenance<-data.frame("Temps Maintenance",Valeur102f)
                  names(TempsMaintenance)<-c("Rubrique","Temps(H)")
                  TempsTotal<-data.frame("Temps Total",Valeur102e)
                  names(TempsTotal)<-c("Rubrique","Temps(H)")
                  TempsCadence<-data.frame("Temps Cadences",Valeur102d)
                  names(TempsCadence)<-c("Rubrique","Temps(H)")
                  TempsRequis<-data.frame("Temps Requis",Valeur102a)
                  names(TempsRequis)<-c("Rubrique","Temps(H)")
                  TempsExogen<-data.frame("Temps Exogene",Valeur102b1)
                  names(TempsExogen)<-c("Rubrique","Temps(H)")
                  TempsMachine<-data.frame("Temps Machine",Valeur102b2)
                  names(TempsMachine)<-c("Rubrique","Temps(H)")
                  TempsFabrication<-data.frame("Temps Fabrication",Valeur102b3)
                  names(TempsFabrication)<-c("Rubrique","Temps(H)")
                  TempsFonctionnement<-data.frame("Temps Fonctionnement",Valeur102c)
                  names(TempsFonctionnement)<-c("Rubrique","Temps(H)")
                  TempsNet<-data.frame("Temps Net",Valeur102)
                  names(TempsNet)<-c("Rubrique","Temps(H)")
                  
                  dataf_Temps<-rbind(TempsExogen,TempsMachine,
                                     TempsTotal,TempsOuverture
                  )
                  dataf_Temps$"Temps(H)"<-formatC(dataf_Temps$"Temps(H)",digits = 2,format ="f",big.mark=' ' )
                  
                  datatable(dataf_Temps, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 11,
                              lengthMenu = c(11),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabTemps4 = renderDT({
                  dataf_Panne<-dataf_Panne()
                  Vdate<-as.Date(paste(VYear,'/',VMonth,'/','01',sep = "")) 
                  Valeur102a<-days_in_month(Vdate)*24-sum(dataf_Panne[dataf_Panne$clasification_machi=="chomer",]$DureePanne)/60-sum(dataf_Panne[dataf_Panne$clasification_machi=="maintenance",]$DureePanne)/60 #Temps requis
                  Valeur102b <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene","machine","fabrication"),]$DureePanne)/60 #heure Exogene, Machine et Fabrication
                  Valeur102b1 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("exogene"),]$DureePanne)/60 #heure Exogene
                  Valeur102b2 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("machine"),]$DureePanne)/60 #heure Machine
                  Valeur102b3 <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("fabrication"),]$DureePanne)/60 #heure Fabrication
                  
                  Valeur102c<-Valeur102a-Valeur102b #temps de fonctionnement
                  Valeur102d <- sum(dataf_Panne[dataf_Panne$clasification_machi=="cadence",]$DureePanne)/60   #Heure cadence
                  Valeur102<-Valeur102c-Valeur102d #Temps Net
                  Valeur102e<-days_in_month(Vdate)*24#Temps Total
                  Valeur102f <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("maintenance"),]$DureePanne)/60 #heure Maintenance
                  Valeur102g <- sum(dataf_Panne[dataf_Panne$clasification_mach %in% c("chomer"),]$DureePanne)/60 #heure Chomer
                  Valeur102h<-Valeur102e-Valeur102g #Temps Ouverture
                  
                  TempsOuverture<-data.frame("Temps Ouverture",Valeur102h)
                  names(TempsOuverture)<-c("Rubrique","Temps(H)")
                  TempsChomer<-data.frame("Temps Chomé",Valeur102g)
                  names(TempsChomer)<-c("Rubrique","Temps(H)")
                  TempsMaintenance<-data.frame("Temps Maintenance",Valeur102f)
                  names(TempsMaintenance)<-c("Rubrique","Temps(H)")
                  TempsTotal<-data.frame("Temps Total",Valeur102e)
                  names(TempsTotal)<-c("Rubrique","Temps(H)")
                  TempsCadence<-data.frame("Temps Cadences",Valeur102d)
                  names(TempsCadence)<-c("Rubrique","Temps(H)")
                  TempsRequis<-data.frame("Temps Requis",Valeur102a)
                  names(TempsRequis)<-c("Rubrique","Temps(H)")
                  TempsExogen<-data.frame("Temps Exogene",Valeur102b1)
                  names(TempsExogen)<-c("Rubrique","Temps(H)")
                  TempsMachine<-data.frame("Temps Machine",Valeur102b2)
                  names(TempsMachine)<-c("Rubrique","Temps(H)")
                  TempsFabrication<-data.frame("Temps Fabrication",Valeur102b3)
                  names(TempsFabrication)<-c("Rubrique","Temps(H)")
                  TempsFonctionnement<-data.frame("Temps Fonctionnement",Valeur102c)
                  names(TempsFonctionnement)<-c("Rubrique","Temps(H)")
                  TempsNet<-data.frame("Temps Net",Valeur102)
                  names(TempsNet)<-c("Rubrique","Temps(H)")
                  
                  dataf_Temps<-rbind(TempsExogen,TempsMachine,
                                     TempsTotal,TempsOuverture
                  )
                  dataf_Temps<-rbind(TempsOuverture,TempsRequis,TempsFonctionnement,TempsNet
                  )
                  
                  dataf_Temps$"Temps(H)"<-formatC(dataf_Temps$"Temps(H)",digits = 2,format ="f",big.mark=' ' )
                  
                  datatable(dataf_Temps, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 11,
                              lengthMenu = c(11),
                              autoWidth = FALSE
                            ))
                  
                })
                
                
                output$GPanneParType <- renderPlotly({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul<-dataf_PanneCumul()[complete.cases(dataf_PanneCumul()$NbPanne),]
                  dataf_PanneCumul<-dataf_PanneCumul %>%
                    group_by(TypePanne) %>%
                    summarize(NbPanne=sum(NbPanne)
                    )
                  
                  dataf_PanneCumul<-arrange(dataf_PanneCumul,-NbPanne) 
                  dataf_PanneCumul = dataf_PanneCumul %>% mutate_if(is.factor,
                                                                        fct_explicit_na,
                                                                        na_level = "NA")
                  
                  dataf_PanneCumul$TypePanne  <- with(dataf_PanneCumul, reorder(TypePanne, -NbPanne))
                  p<- ggplot(data=dataf_PanneCumul, aes(x = reorder(TypePanne, NbPanne), y = NbPanne, fill = TypePanne)) + 
                    geom_bar(stat = "identity")+ 
                    coord_flip()+
                    labs(x = "TypePanne", 
                         y = "Nbre de Pannes"
                    )+
                    theme(axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
                  #theme_minimal()
                  ggplotly(p)
                  
                }
                )
                
                output$GEvolutionPannesParMois <- renderPlotly({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul<-dataf_PanneCumul %>%
                    group_by(DateOpe) %>%
                    summarize(NbPanne=sum(NbPanne)
                    )
                  if(nrow(dataf_PanneCumul)>0){
                    p<-ggplot(data=dataf_PanneCumul,aes(x = DateOpe)) + 
                      geom_line(aes(y=NbPanne)) +
                      ylab('Nbre de Pannes')+xlab('Date')
                    
                  }
                }
                )
                
                output$MatricePanne <- renderPivottabler({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul$Libelle<-"Mois"
                  dataf_PanneCumul<-dataf_PanneCumul[,c("Month","MonthName","TypePanne","NbPanne","Libelle")]
                  dataf_PanneCumul <- dataf_PanneCumul[order(dataf_PanneCumul$Month),c("Month","MonthName","TypePanne","NbPanne","Libelle")]
                  
                  pt <- PivotTable$new()
                  pt$addData(dataf_PanneCumul)
                  pt$addColumnDataGroups("Libelle",addTotal=TRUE)
                  pt$addColumnDataGroups("Month",addTotal=FALSE)
                  pt$addColumnDataGroups("MonthName",addTotal=FALSE)
                  pt$addRowDataGroups("TypePanne",outlineTotal=list(groupStyleDeclarations=list(color="blue")))
                  pt$defineCalculation(calculationName="NbPanne", summariseExpression="sum(NbPanne)")
                  pt$renderPivot()
                  pivottabler(pt)
                }
                )
                
                output$MatricePanneClassif <- renderPivottabler({
                  dataf_PanneCumul<-dataf_PanneCumul12Mois()
                  dataf_PanneCumul$Libelle<-"Mois"
                  dataf_PanneCumul<-dataf_PanneCumul[,c("Month","MonthName","clasification_machi","NbPanne","Libelle")]
                  dataf_PanneCumul <- dataf_PanneCumul[order(dataf_PanneCumul$Month),c("Month","MonthName","clasification_machi","NbPanne","Libelle")]
                  
                  pt <- PivotTable$new()
                  pt$addData(dataf_PanneCumul)
                  pt$addColumnDataGroups("Libelle",addTotal=TRUE)
                  pt$addColumnDataGroups("Month",addTotal=FALSE)
                  pt$addColumnDataGroups("MonthName",addTotal=FALSE)
                  pt$addRowDataGroups("clasification_machi",outlineTotal=list(groupStyleDeclarations=list(color="blue")))
                  pt$defineCalculation(calculationName="NbPanne", summariseExpression="sum(NbPanne)")
                  pt$renderPivot()
                  pivottabler(pt)
                }
                )
                
                output$MatriceDureePanne <- renderPivottabler({
                  dataf_PanneCumul<-dataf_PanneCumul()
                  dataf_PanneCumul$Libelle<-"Mois"
                  dataf_PanneCumul<-dataf_PanneCumul[,c("Month","MonthName","TypePanne","DureePanne","Libelle")]
                  dataf_PanneCumul <- dataf_PanneCumul[order(dataf_PanneCumul$Month),c("Month","MonthName","TypePanne","DureePanne","Libelle")]
                  
                  pt <- PivotTable$new()
                  pt$addData(dataf_PanneCumul)
                  pt$addColumnDataGroups("Libelle",addTotal=TRUE)
                  pt$addColumnDataGroups("Month",addTotal=FALSE)
                  pt$addColumnDataGroups("MonthName",addTotal=FALSE)
                  pt$addRowDataGroups("TypePanne",outlineTotal=list(groupStyleDeclarations=list(color="blue")))
                  pt$defineCalculation(calculationName="DureePanne", summariseExpression="round(sum(DureePanne)/60,digits=1)")
                  pt$renderPivot()
                  pivottabler(pt)
                }
                )
                
        #----------------- RESSOURCES HUMAINES --------------------------------------------------------------
                
                
                output$value22 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur22 <- sum(dataf_Effectif$NbEffectif)
                  valueBox(formatC(Valeur22,digits = 0,format ="f",big.mark=' ' ),'Effectif du Mois',color = "black")
                })     
                
                output$value23 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  dataf_Effectif<-dataf_Effectif %>% filter(SEXE == 'M')
                  Valeur23 <- sum(dataf_Effectif$NbEffectif)
                  valueBox(formatC(Valeur23,digits = 0,format ="f",big.mark=' ' ),'HOMMES',color = "blue")
                })   
                
                output$value24 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  dataf_Effectif<-dataf_Effectif %>% filter(SEXE != 'M')
                  Valeur24 <- sum(dataf_Effectif$NbEffectif)
                  valueBox(formatC(Valeur24,digits = 0,format ="f",big.mark=' ' ),'FEMMES',color = "black")
                })   
                
                output$value25 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur25a <- sum(dataf_Effectif$NbEffectif)
                  dataf_Effectif<-dataf_Effectif %>% filter(SEXE != 'M')
                  Valeur25b <- sum(dataf_Effectif$NbEffectif)
                  valeur25<-(Valeur25b/Valeur25a)*100
                  valueBox(formatC(valeur25,digits = 2,format ="f",big.mark=' ' ),'%FEMMES',color = "orange")
                })
                
                output$value26 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbEffectif=sum(NbEffectif)
                    )
                  Valeur26 <- mean(dataf_EffectifCumul$NbEffectif)
                  valueBox(formatC(Valeur26,digits = 0,format ="f",big.mark=' ' ),'CumulAn Moy. Effectif',color = "black")
                })
                
                output$value27 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(SEXE == 'M')
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbEffectif=sum(NbEffectif)
                    )
                  
                  Valeur27 <- mean(dataf_EffectifCumul$NbEffectif)
                  valueBox(formatC(Valeur27,digits = 0,format ="f",big.mark=' ' ),'Cumul An - HOMMES',color = "blue")
                })
                
                output$value28 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(SEXE != 'M')
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbEffectif=sum(NbEffectif)
                    )
                  Valeur28 <- mean(dataf_EffectifCumul$NbEffectif)
                  valueBox(formatC(Valeur28,digits = 0,format ="f",big.mark=' ' ),'Cumul An - FEMMES',color = "black")
                })
                
                output$value29 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumulTotal<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbEffectif=sum(NbEffectif)
                    )
                  Valeur29b <- mean(dataf_EffectifCumulTotal$NbEffectif)
                  
                  dataf_EffectifCumulFemme<-dataf_EffectifCumul %>% filter(SEXE != 'M')
                  dataf_EffectifCumulFemme<-dataf_EffectifCumulFemme %>%
                    group_by(MonthName) %>%
                    summarize(NbEffectif=sum(NbEffectif)
                    )
                  Valeur29a <- mean(dataf_EffectifCumulFemme$NbEffectif)
                  
                  valeur29<-(Valeur29a/Valeur29b)*100
                  valueBox(formatC(valeur29,digits = 2,format ="f",big.mark=' ' ),'% Cumul An. FEMMES.',color = "orange")
                })
              
                output$value30 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur30 <- sum(dataf_Effectif$NbPermanent)
                  valueBox(formatC(Valeur30,digits = 0,format ="f",big.mark=' ' ),'Permanents du Mois',color = "blue")
                })
                
                output$value31 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur31 <- sum(dataf_Effectif$NbSaisonnier)
                  valueBox(formatC(Valeur31,digits = 0,format ="f",big.mark=' ' ),'Saisonniers du Mois',color = "red")
                })
                
                output$value32 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur32 <- sum(dataf_Effectif$NbTemporaire)
                  valueBox(formatC(Valeur32,digits = 0,format ="f",big.mark=' ' ),'Temporaires du Mois',color = "yellow")
                })
                
                output$value33 <- renderValueBox({
                  dataf_Effectif<-dataf_Effectif()
                  Valeur33 <- sum(dataf_Effectif$NbGardien)
                  valueBox(formatC(Valeur33,digits = 0,format ="f",big.mark=' ' ),'Gardiens du Mois',color = "green")
                })
                
                
                output$value34 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbPermanent=sum(NbPermanent)
                    )
                  Valeur34 <- mean(dataf_EffectifCumul$NbPermanent)
                  valueBox(formatC(Valeur34,digits = 0,format ="f",big.mark=' ' ),'CumulAn Moy. Permanents ',color = "blue")
                })
                
                output$value35 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbSaisonnier=sum(NbSaisonnier)
                    )
                  Valeur35 <- mean(dataf_EffectifCumul$NbSaisonnier)
                  valueBox(formatC(Valeur35,digits = 0,format ="f",big.mark=' ' ),'CumulAn Moy.',color = "red")
                })
                
                output$value36 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbTemporaire=sum(NbTemporaire)
                    )
                  Valeur36 <- mean(dataf_EffectifCumul$NbTemporaire)
                  valueBox(formatC(Valeur36,digits = 0,format ="f",big.mark=' ' ),'CumulAn Moy.',color = "yellow")
                })
                
                output$value37 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName) %>%
                    summarize(NbGardien=sum(NbGardien)
                    )
                  Valeur37 <- mean(dataf_EffectifCumul$NbGardien)
                  valueBox(formatC(Valeur37,digits = 0,format ="f",big.mark=' ' ),'CumulAn Moy. Gardiens',color = "green")
                })
                
                
                output$GEvolutionEffectif <- renderPlotly({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(DateOpe) %>%
                    summarize(TotalEffectif=sum(NbEffectif),
                              Homme=sum( ifelse( SEXE == "M", NbEffectif, 0 ),na.rm = TRUE),
                              Femme=sum( ifelse( SEXE != "M", NbEffectif, 0 ),na.rm = TRUE)
                    )
                  
                  #dataf_productionUsine['Objectif'] = 43
                  
                  if(nrow(dataf_EffectifCumul)>0){
                    p<-ggplot(data=dataf_EffectifCumul,aes(x = DateOpe)) + 
                      geom_line(aes(y=TotalEffectif,colour ="TotalEffectif")) + 
                      geom_line(aes(y=Homme,colour ="Homme")) + 
                      geom_line(aes(y=Femme,colour ="Femme")) +
                      scale_colour_manual("", 
                                          breaks = c("TotalEffectif", "Homme", "Femme"),
                                          values = c("green", "blue", "black"))+ 
                    
                      ylab('Effectif')+xlab('date')
                      
                  }
                }
                )
                
                
                output$GPyramideAge <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbEffectif!=0,],aes(Age))+
                      geom_histogram(binwidth = 1,color='red',fill='pink',alpha=0.4)+
                      #geom_histogram(binwidth = 1,aes(fill=..count..))+                
                      ylab('Effectif')+xlab('Age')
                    
                  }
                }
                )
                
                output$GPyramideAgeSexeTotal <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbEffectif!=0,],aes(Age))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Age')
                    
                  }
                }
                )
                
                output$GPyramideAgeSexeSaisonnier <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbSaisonnier!=0,],aes(Age))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Age')
                    
                  }
                }
                )
                output$GPyramideAgeSexePermanent <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbPermanent!=0,],aes(Age))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Age')
                    
                  }
                }
                )
                output$GPyramideAgeSexeTemporaraire <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbTemporaire!=0,],aes(Age))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Age')
                    
                  }
                }
                )
                
                
              #---
                output$GPyramideAncSexeTotal <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbEffectif!=0,],aes(Anc))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Anciennete')
                    
                  }
                }
                )
                
                output$GPyramideAncSexeSaisonnier <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbSaisonnier!=0,],aes(Anc))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Anciennete')
                    
                  }
                }
                )
                output$GPyramideAncSexePermanent <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbPermanent!=0,],aes(Anc))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Anciennete')
                    
                  }
                }
                )
                output$GPyramideAncSexeTemporaraire <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  if(nrow(dataf_Effectif)>0){
                    ggplot(dataf_Effectif[dataf_Effectif$NbTemporaire!=0,],aes(Anc))+
                      geom_histogram(binwidth = 1,aes(fill=SEXE))+
                      ylab('Effectif')+xlab('Anciennete')
                    
                  }
                }
                )
                
              #---
                
                
                output$GEvolutionClasseEffectif <- renderPlotly({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(DateOpe) %>%
                    summarize(TotalEffectif=sum(NbEffectif),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  
                  
                  if(nrow(dataf_EffectifCumul)>0){
                    p<-ggplot(data=dataf_EffectifCumul,aes(x = DateOpe)) + 
                      geom_line(aes(y=TotalEffectif,colour ="TotalEffectif")) + 
                      geom_line(aes(y=Permanents,colour ="Permanents")) + 
                      geom_line(aes(y=Saisonniers,colour ="Saisonniers")) +
                      geom_line(aes(y=Temporaires,colour ="Temporaires")) +
                      geom_line(aes(y=Gardiens,colour ="Gardiens")) +
                      scale_colour_manual("", 
                                          breaks = c("TotalEffectif", "Permanents", "Saisonniers","Temporaires","Gardiens"),
                                          values = c("black", "blue", "red","yellow","green"))+ 
                      
                      ylab('Effectif')+xlab('date')
                    
                  }
                }
                )
                
                output$GEffectifparSexe <- renderPlotly({
                  dataf_Effectif<-dataf_Effectif()
                  EffectifTotal<-dataf_Effectif %>%
                    group_by(SEXE) %>%
                    summarize(Effectif=sum(NbEffectif)
                              )
                  EffectifTotal$Classe<-"Total"
                  
                  EffectifPermanent<-dataf_Effectif %>%
                    group_by(SEXE) %>%
                    summarize(Effectif=sum(NbPermanent)
                    )
                  EffectifPermanent$Classe<-"Permanents"
                  
                  EffectifSaisonnier<-dataf_Effectif %>%
                    group_by(SEXE) %>%
                    summarize(Effectif=sum(NbSaisonnier)
                    )
                  EffectifSaisonnier$Classe<-"Saisonniers"
                  
                  EffectifTemporaire<-dataf_Effectif %>%
                    group_by(SEXE) %>%
                    summarize(Effectif=sum(NbTemporaire)
                    )
                  EffectifTemporaire$Classe<-"Temporaires"
                  
                  EffectifGardien<-dataf_Effectif %>%
                    group_by(SEXE) %>%
                    summarize(Effectif=sum(NbGardien)
                    )
                  EffectifGardien$Classe<-"Gardiens"
                  
                  
                  dataf_Effectif<-rbind(EffectifTotal,
                                        EffectifPermanent,
                                        EffectifSaisonnier,
                                        EffectifTemporaire,
                                        EffectifGardien
                                        )
                  names(dataf_Effectif)<-c("SEXE","Effectif","Classe")
              
                  if(nrow(dataf_Effectif)>0){
                    p<-ggplot(data=dataf_Effectif, aes(x = Classe , y = Effectif,fill=SEXE)) + 
                      geom_bar(stat = "identity")+ 
                      labs(x = "Classe", 
                           y = "Effectif",
                           title = "Effectif Mensuel par classe de salariés et par sexe")+ 
                      theme_minimal()
                    ggplotly(p)
                  }    
                  
                }
                )
                
                
                
                output$TabEffectif = renderDT({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(TotalEffectif=sum(NbEffectif),
                              Homme=sum( ifelse( SEXE == "M", NbEffectif, 0 ),na.rm = TRUE),
                              Femme=sum( ifelse( SEXE != "M", NbEffectif, 0 ),na.rm = TRUE),
                              Ratio=sum( ifelse( SEXE != "M", NbEffectif, 0 ),na.rm = TRUE)*100/sum(NbEffectif)
                    )
                  dataf_EffectifCumul$Ratio<-formatC(dataf_EffectifCumul$Ratio,digits = 2,format ="f",big.mark=',',width = 2)
                  names(dataf_EffectifCumul)<-c("Mois","Month","TotalEffectif","Homme","Femme","%Femme")
                  dataf_EffectifCumul <- dataf_EffectifCumul[order(dataf_EffectifCumul$Month),c("Mois","Homme","Femme","TotalEffectif","%Femme")]
                  
                  datatable(dataf_EffectifCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
               })
                
                output$TabEffectifParClasse = renderDT({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(TotalEffectif=sum(NbEffectif),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  names(dataf_EffectifCumul)<-c("Mois","Month","TotalEffectif","Permanents","Saisonniers","Temporaires","Gardiens")
                  dataf_EffectifCumul <- dataf_EffectifCumul[order(dataf_EffectifCumul$Month),c("Mois","TotalEffectif","Permanents","Saisonniers","Temporaires","Gardiens")]
                  
                  datatable(dataf_EffectifCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabEffectifParDirection = renderDT({
                  dataf_Effectif<-dataf_Effectif()
                  dataf_Effectif<-dataf_Effectif %>%
                    group_by(Direction) %>%
                    summarize(TotalEffectif=sum(NbEffectif),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  datatable(dataf_Effectif, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 9,
                              lengthMenu = c(9),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabEffectifCatSocio = renderDT({
                  dataf_Effectif<-dataf_Effectif()
                  EffectifCadre<-data.frame("Cadres",sum(dataf_Effectif$NbCadre))
                  names(EffectifCadre)<-c("CateSocio","Effectif")
                  EffectifAgm<-data.frame("Agents de Maitrise",sum(dataf_Effectif$NbAgm))
                  names(EffectifAgm)<-c("CateSocio","Effectif")
                  EffectifEmp<-data.frame("Employés & Ouvriers",sum(dataf_Effectif$NbEmp))
                  names(EffectifEmp)<-c("CateSocio","Effectif")
                  EffectifFoncAffecte<-data.frame("Fonctionnaires Affectés",sum(dataf_Effectif$NbFonctAffecte))
                  names(EffectifFoncAffecte)<-c("CateSocio","Effectif")
                  EffectifFoncDetache<-data.frame("Fonctionnaires Détachés",sum(dataf_Effectif$NbFonctDetache))
                  names(EffectifFoncDetache)<-c("CateSocio","Effectif")
                  EffectifHomme<-data.frame("Hommes",sum( (dataf_Effectif %>% filter(SEXE == 'M'))$NbEffectif))
                  names(EffectifHomme)<-c("CateSocio","Effectif")
                  EffectifFemme<-data.frame("Femmes",sum( (dataf_Effectif %>% filter(SEXE != 'M'))$NbEffectif))
                  names(EffectifFemme)<-c("CateSocio","Effectif")
                  EffectifExpat<-data.frame("Expatriés",sum(dataf_Effectif$NbExpat))
                  names(EffectifExpat)<-c("CateSocio","Effectif")
                  EffectifLocaux<-data.frame("Locaux",sum(dataf_Effectif$NbLocaux))
                  names(EffectifLocaux)<-c("CateSocio","Effectif")
                  
                  dataf_Effectif<-rbind(EffectifCadre,EffectifAgm,EffectifEmp,EffectifFoncAffecte,EffectifFoncDetache,EffectifHomme,EffectifFemme,EffectifExpat,EffectifLocaux)
            
                  datatable(dataf_Effectif, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 9,
                              lengthMenu = c(9),
                              autoWidth = FALSE
                            ))
                  
                })
                
                
                output$MatriceEffectif <- renderRpivotTable( 
                  rpivotTable::rpivotTable( 
                    rows = c("MonthName"),
                    #cols=c("CAT"),
                    vals =  c("NbEffectif","NbPermanent"),
                    aggregatorName = "Sum",
                    exclusions = "totals",
                    rendererName ="Table",width="50%", height="550px",
                    {
                      data <- dataf_EffectifCumul()
                     
                      data<-data[,c("SEXE","Direction","MonthName","NbEffectif","NbPermanent")]
                      data
                    }))
                
                
                output$value38 <- renderValueBox({
                  dataf_Depart<-dataf_Depart()
                  Valeur38 <- sum(dataf_Depart$NbDepart)
                  valueBox(formatC(Valeur38,digits = 0,format ="f",big.mark=' ' ),'Total Departs-Mois',color = "black")
                })
                
                output$value39 <- renderValueBox({
                  dataf_Depart<-dataf_Depart()
                  Valeur39 <- sum(dataf_Depart$NbPermanent)
                  valueBox(formatC(Valeur39,digits = 0,format ="f",big.mark=' ' ),'Permanents',color = "blue")
                })
                
                output$value40 <- renderValueBox({
                  dataf_Depart<-dataf_Depart()
                  Valeur40 <- sum(dataf_Depart$NbSaisonnier)
                  valueBox(formatC(Valeur40,digits = 0,format ="f",big.mark=' ' ),'Saisonniers',color = "red")
                })
                
                output$value41 <- renderValueBox({
                  dataf_Depart<-dataf_Depart()
                  Valeur41 <- sum(dataf_Depart$NbTemporaire)
                  valueBox(formatC(Valeur41,digits = 0,format ="f",big.mark=' ' ),'Temporaires',color = "yellow")
                })
                
                output$value42 <- renderValueBox({
                  dataf_Depart<-dataf_Depart()
                  Valeur42 <- sum(dataf_Depart$NbGardien)
                  valueBox(formatC(Valeur42,digits = 0,format ="f",big.mark=' ' ),'Gardiens',color = "green")
                })
                
                output$TabDepartParClasse = renderDT({
                  dataf_DepartCumul<-dataf_DepartCumul()
                  dataf_DepartCumul<-dataf_DepartCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(TotalDepart=sum(NbDepart),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  names(dataf_DepartCumul)<-c("Mois","Month","Total","Perm.","Saiso.","Tempo.","Gardiens")
                  dataf_DepartCumul <- dataf_DepartCumul[order(dataf_DepartCumul$Month),c("Mois","Total","Perm.","Saiso.","Tempo.","Gardiens")]
                  
                  datatable(dataf_DepartCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              columnDefs = list(list(width = '10px', targets = c(1,3))),
                              autoWidth = TRUE
                            ))
                  
                })
                
                output$TabDepartParDirection = renderDT({
                  dataf_Depart<-dataf_Depart()
                  dataf_Depart<-dataf_Depart %>%
                    group_by(Direction) %>%
                    summarize(Total=sum(NbDepart),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  datatable(dataf_Depart, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 5,
                              lengthMenu = c(5),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabDepartParMotif = renderDT({
                  dataf_Depart<-dataf_Depart()
                  dataf_Depart<-dataf_Depart %>%
                    group_by(MotifDepart) %>%
                    summarize(Total=sum(NbDepart),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire)#,
                              #Gardiens=sum(NbGardien)
                    )
                  datatable(dataf_Depart, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 7,
                              lengthMenu = c(7),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabDepartParMotifCumul = renderDT({
                  dataf_DepartCumul<-dataf_DepartCumul()
                  dataf_DepartCumul<-dataf_DepartCumul %>%
                    group_by(MotifDepart) %>%
                    summarize(Total=sum(NbDepart),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire)#,
                              #Gardiens=sum(NbGardien)
                    )
                  datatable(dataf_DepartCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                output$value43 <- renderValueBox({
                  dataf_Recrutement<-dataf_Recrutement()
                  Valeur43 <- sum(dataf_Recrutement$NbRecrutement)
                  valueBox(formatC(Valeur43,digits = 0,format ="f",big.mark=' ' ),'Total Recrutement-Mois',color = "black")
                })
                
                output$value44 <- renderValueBox({
                  dataf_Recrutement<-dataf_Recrutement()
                  Valeur44 <- sum(dataf_Recrutement$NbPermanent)
                  valueBox(formatC(Valeur44,digits = 0,format ="f",big.mark=' ' ),'Permanents',color = "blue")
                })
                
                output$value45 <- renderValueBox({
                  dataf_Recrutement<-dataf_Recrutement()
                  Valeur45 <- sum(dataf_Recrutement$NbSaisonnier)
                  valueBox(formatC(Valeur45,digits = 0,format ="f",big.mark=' ' ),'Saisonniers',color = "red")
                })
                
                output$value46 <- renderValueBox({
                  dataf_Recrutement<-dataf_Recrutement()
                  Valeur46 <- sum(dataf_Recrutement$NbTemporaire)
                  valueBox(formatC(Valeur46,digits = 0,format ="f",big.mark=' ' ),'Temporaires',color = "yellow")
                })
                
                output$value47 <- renderValueBox({
                  dataf_Recrutement<-dataf_Recrutement()
                  Valeur47 <- sum(dataf_Recrutement$NbGardien)
                  valueBox(formatC(Valeur47,digits = 0,format ="f",big.mark=' ' ),'Gardiens',color = "green")
                })
                
                
                output$TabRecrutementParClasse = renderDT({
                  dataf_RecrutementCumul<-dataf_RecrutementCumul()
                  dataf_RecrutementCumul<-dataf_RecrutementCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(TotalRecrutement=sum(NbRecrutement),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  names(dataf_RecrutementCumul)<-c("Mois","Month","Total","Perm.","Saiso.","Tempo.","Gardiens")
                  dataf_RecrutementCumul <- dataf_RecrutementCumul[order(dataf_RecrutementCumul$Month),c("Mois","Total","Perm.","Saiso.","Tempo.","Gardiens")]
                  
                  datatable(dataf_RecrutementCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 5,
                              lengthMenu = c(5),
                              autoWidth = TRUE
                            ))
                  
                })
                
                output$TabRecrutementParDirection = renderDT({
                  dataf_Recrutement<-dataf_Recrutement()
                  dataf_Recrutement<-dataf_Recrutement %>%
                    group_by(Direction) %>%
                    summarize(Total=sum(NbRecrutement),
                              Permanents=sum(NbPermanent),
                              Saisonniers=sum(NbSaisonnier),
                              Temporaires=sum(NbTemporaire),
                              Gardiens=sum(NbGardien)
                    )
                  datatable(dataf_Recrutement, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$value48 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(Month == VMonthPrec)
                  valeur48<-sum(dataf_EffectifCumul$NbEffectif)
                  valueBox(formatC(valeur48,digits = 0,format ="f",big.mark=' ' ),'Effectif-Mois Prec.',color = "black")
                })
                output$value49 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(Month == VMonthPrec)
                  valeur49<-sum(dataf_EffectifCumul$NbPermanent)
                  valueBox(formatC(valeur49,digits = 0,format ="f",big.mark=' ' ),'Permanents-Mois Prec.',color = "blue")
                })
                output$value50 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(Month == VMonthPrec)
                  valeur50<-sum(dataf_EffectifCumul$NbSaisonnier)
                  valueBox(formatC(valeur50,digits = 0,format ="f",big.mark=' ' ),'Saisonniers-Mois Prec.',color = "red")
                })
                output$value51 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(Month == VMonthPrec)
                  valeur51<-sum(dataf_EffectifCumul$NbTemporaire)
                  valueBox(formatC(valeur51,digits = 0,format ="f",big.mark=' ' ),'Temporaires-Mois Prec.',color = "yellow")
                })
                output$value52 <- renderValueBox({
                  dataf_EffectifCumul<-dataf_EffectifCumul()
                  dataf_EffectifCumul<-dataf_EffectifCumul %>% filter(Month == VMonthPrec)
                  valeur52<-sum(dataf_EffectifCumul$NbGardien)
                  valueBox(formatC(valeur52,digits = 0,format ="f",big.mark=' ' ),'Gardiens-Mois Prec.',color = "green")
                })
    
                
                
                output$TabEffectifParAge = renderDT({
                  dataf_Effectif<-dataf_Effectif()
                  dataf_Effectif<-dataf_Effectif[dataf_Effectif$NbEffectif>0,]
                  dataf_Effectif<-dataf_Effectif %>%
                    group_by(Direction) %>%
                    summarize(K=sum(de0a20An),
                              A=sum(de21a30An),
                              B=sum(de31a40An),
                              C=sum(de41a50An),
                              D=sum(de51a60An),
                              E=sum(de61a70An),
                              G=sum(plus70An)
                              
                    )
                  names(dataf_Effectif)<-c("Direction","0-20ans","21-30ans","31-40ans","41-50ans","51-60ans","61-70ans","+70ans")
                  #dataf_Effectif <- dataf_Effectif[order(dataf_EffectifCumul$Month),c("Mois","TotalEffectif","Permanents","Saisonniers","Temporaires","Gardiens")]
                  datatable(dataf_Effectif, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabEffectifParAge2 = renderDT({
                  dataf_Effectif<-dataf_Effectif()
                  dataf_Effectif<-dataf_Effectif[dataf_Effectif$NbEffectif>0,]
                  dataf_Effectif<-dataf_Effectif %>% filter(Age >= input$AgeDebut & Age <= input$AgeFin)
                  dataf_Effectif<-dataf_Effectif %>%
                    group_by(Direction) %>%
                    summarize(Effectif=sum(de0a20An +
                                             de21a30An +
                                             de31a40An +
                                             de41a50An +
                                             de51a60An +
                                             de61a70An +
                                             plus70An
                                             )
                    )
                  datatable(dataf_Effectif, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 4,
                              lengthMenu = c(4),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$value67 <- renderValueBox({
                  dataf_Mission<-dataf_Mission()
                  Valeur67 <- sum(dataf_Mission$NbMission)
                  valueBox(formatC(Valeur67,digits = 0,format ="f",big.mark=' ' ),'Nbre de Missions du Mois',color = "black")
                }) 
                
                output$value68 <- renderValueBox({
                  dataf_Mission<-dataf_Mission()
                  Valeur68 <- sum(dataf_Mission$NbjourMission)
                  valueBox(formatC(Valeur68,digits = 0,format ="f",big.mark=' ' ),'Jours Misssion du Mois',color = "yellow")
                })
                output$value69 <- renderValueBox({
                  dataf_Mission<-dataf_Mission()
                  Valeur69 <- sum(dataf_Mission$MontantMission)
                  valueBox(formatC(Valeur69,digits = 0,format ="f",big.mark=' ' ),'Montant Mois (FCFA)',color = "green")
                })
                output$value70 <- renderValueBox({
                  dataf_Mission<-dataf_Mission()
                  Valeur70 <- sum(dataf_Mission$MontantMission)/sum(dataf_Mission$NbjourMission)
                  valueBox(formatC(Valeur70,digits = 0,format ="f",big.mark=' ' ),'Cout Moy. Jour Mission du Mois (FCFA)',color = "blue")
                })
                
                output$TabMissionParMois = renderDT({
                  dataf_MissionCumul<-dataf_MissionCumul()
                  dataf_MissionCumul<-dataf_MissionCumul %>%
                    group_by(MonthName,Month) %>%
                    summarize(NbMissions=sum(NbMission),
                              NbjourMission=sum(NbjourMission),
                              MontantMission=sum(MontantMission)#,
                              #CoutMoyJour=sum(MontantMission)/sum(NbjourMission)
                             
                    )
                  dataf_MissionCumul$MontantMission<-formatC(dataf_MissionCumul$MontantMission,digits = 0,format ="f",big.mark=' ',width = 2)
                  #dataf_MissionCumul$CoutMoyJour<-formatC(dataf_MissionCumul$CoutMoyJour,digits = 0,format ="f",big.mark=' ',width = 2)
                  
                  names(dataf_MissionCumul)<-c("Mois","Month","NbMissions","Nbjours","Montant(CFA)"#,"CoutJour(CFA)"
                                               )
                  dataf_MissionCumul <- dataf_MissionCumul[order(dataf_MissionCumul$Month),c("Mois","NbMissions","Nbjours","Montant(CFA)"#,"CoutJour(CFA)"
                                                                                             )]
                  
                  datatable(dataf_MissionCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabMissionParDirection = renderDT({
                  dataf_MissionCumul<-dataf_MissionCumul()
                  dataf_MissionCumul<-dataf_MissionCumul %>%
                    group_by(Direction) %>%
                    summarize(NbMissions=sum(NbMission),
                              NbjourMission=sum(NbjourMission),
                              MontantMission=sum(MontantMission)#,
                              #CoutMoyJour=sum(MontantMission)/sum(NbjourMission)
                    )
                  dataf_MissionCumul$MontantMission<-formatC(dataf_MissionCumul$MontantMission,digits = 0,format ="f",big.mark=' ',width = 2)
                  #dataf_MissionCumul$CoutMoyJour<-formatC(dataf_MissionCumul$CoutMoyJour,digits = 0,format ="f",big.mark=' ',width = 2)
                  names(dataf_MissionCumul)<-c("Direction","NbMissions","Nbjours","Montant(CFA)"#,"CoutJour(CFA)"
                                               )
                  
                  datatable(dataf_MissionCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 6,
                              lengthMenu = c(6),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$GMissionParDirection <- renderPlotly({
                  dataf_MissionCumul<-dataf_MissionCumul()
                  dataf_MissionCumul<-dataf_MissionCumul()[complete.cases(dataf_MissionCumul()$MontantMission),]
                  dataf_MissionCumul<-dataf_MissionCumul %>%
                    group_by(Direction) %>%
                    summarize(MontantMission=sum(MontantMission)/1000000
                    )
                  
                  dataf_MissionCumul<-arrange(dataf_MissionCumul,-MontantMission) 
                  dataf_MissionCumul = dataf_MissionCumul %>% mutate_if(is.factor,
                                                                    fct_explicit_na,
                                                                    na_level = "NA")
                  
                  dataf_MissionCumul$Direction  <- with(dataf_MissionCumul, reorder(Direction, -MontantMission))
                  p<- ggplot(data=dataf_MissionCumul, aes(x = reorder(Direction, MontantMission), y = MontantMission, fill = Direction)) + 
                    geom_bar(stat = "identity")+ 
                    coord_flip()+
                    labs(x = "Direction", 
                         y = "Montant(MillionsFCFA)"
                    )+
                    theme(axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank())
                  #theme_minimal()
                  ggplotly(p)
                  
                }
                )
                
                output$GEvolutionMissionParMois <- renderPlotly({
                  dataf_MissionCumul<-dataf_MissionCumul()
                  dataf_MissionCumul<-dataf_MissionCumul %>%
                    group_by(DateOpe) %>%
                    summarize(MontantMission=sum(MontantMission)/1000,
                              CoutMoyJour=sum(MontantMission)/sum(NbjourMission)/1000
                    )
                   if(nrow(dataf_MissionCumul)>0){
                    p<-ggplot(data=dataf_MissionCumul,aes(x = DateOpe)) + 
                      geom_line(aes(y=MontantMission)) +#,colour ="MontantMission")) + 
                      #geom_line(aes(y=CoutMoyJour,colour ="CoutMoyJour")) +                      scale_colour_manual("", 
                      #                    breaks = c("MontantMission", "CoutMoyJour"
                      #                               ),
                      #                    values = c("black", "green"))+ 
                      
                      ylab('Montant(kfca)')+xlab('Date')
                    
                  }
                }
                )
                
                
      #----------------- STock HUILERIE --------------------------------------------------------------
                
                output$value53 <- renderValueBox({
                  dataf_Stock<-dataf_Stock()
                  nb<-nrow(dataf_Stock)
                  if(nb!=0){
                    valeur53<-sum(dataf_Stock$Qte)
                  }else{
                    valeur53<-0
                  }
                  valueBox(formatC(valeur53,digits = 0,format ="f",big.mark=' ' ),'QteStock (Litre)',color = "black")
                })
                output$value54 <- renderValueBox({
                  dataf_Stock<-dataf_Stock()
                  nb<-nrow(dataf_Stock)
                  if(nb!=0){
                    valeur54<-sum(dataf_Stock$Qte)
                  }else{
                    valeur54<-0
                  }
                  valueBox(formatC(valeur54,digits = 0,format ="f",big.mark=' ' ),'Moantant (FCFA)',color = "green")
                })
                
                output$TabQteMontantParProduit = renderDT({
                  dataf_Stock<-dataf_Stock()
                  dataf_Stock<-dataf_Stock %>%
                    group_by(LIBART1) %>%
                    summarize(Qte=sum(Qte),
                              Montant=sum(Montant)
                              
                    )
                  names(dataf_Stock)<-c("Article","Qte(L)","Montant(FCFA)")
                  datatable(dataf_Stock, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 5,
                              lengthMenu = c(5),
                              autoWidth = FALSE
                            ))
                  
                })
                
                output$TabStockEvolution = renderDT({
                  dataf_StockCumul<-dataf_StockCumul()
                  dataf_StockCumul<-dataf_StockCumul %>%
                    group_by(MonthName,Month,LIBART1) %>%
                    summarize(Qte=sum(Qte),
                              Montant=sum(Montant)
                    )
                  names(dataf_StockCumul)<-c("Mois","Month","Article","Qte(L)","Montant(FCFA)")
                  dataf_StockCumul <- dataf_StockCumul[order(dataf_StockCumul$Month),c("Mois","Article","Qte(L)","Montant(FCFA)")]
                  
                  datatable(dataf_StockCumul, rownames = FALSE,selection = 'single',
                            options=list(
                              dom = 'tp',
                              pageLength = 5,
                              lengthMenu = c(5),
                              autoWidth = TRUE
                            ))
                  
                })
                
                output$MatriceStock <- renderRpivotTable( 
                  rpivotTable::rpivotTable( 
                    rows = c("Month","MonthName"),
                    cols=c("LIBART1"),
                    vals =  c("Qte","Montant"),
                    aggregatorName = "Sum",
                    rendererName ="Table",
                    {
                      data <- dataf_StockCumul()
                      data
                    },
                    autoWidth = FALSE,
                    exclusions=list( Class = list( "Totals")),
                    subtotals =TRUE
                    )
                  )
                
                
                output$MatriceStock2 <- renderPivottabler({
                  dataf_StockCumul<-dataf_StockCumul()
                  dataf_StockCumul$Libelle<-"Mois"
                  dataf_StockCumul<-dataf_StockCumul[,c("Month","MonthName","LIBART1","Qte","Libelle")]
                  dataf_StockCumul <- dataf_StockCumul[order(dataf_StockCumul$Month),c("Month","MonthName","LIBART1","Qte","Libelle")]
                  
                  pt <- PivotTable$new()
                  pt$addData(dataf_StockCumul)
                  pt$addColumnDataGroups("Libelle",addTotal=FALSE)
                  pt$addColumnDataGroups("Month",addTotal=FALSE)
                  pt$addColumnDataGroups("MonthName",addTotal=FALSE)
                  pt$addRowDataGroups("LIBART1",outlineTotal=list(groupStyleDeclarations=list(color="blue")))
                  pt$defineCalculation(calculationName="Qte", summariseExpression="sum(Qte)")
                  pt$renderPivot()
                  pivottabler(pt)
                }
                )
                
                 output$TabStockEvolution2 = renderPrint({
                   dataf_Stock <- dataf_StockCumul()
                   dataf_Stock<-xtabs(Qte~LIBART1+MonthName,dataf_Stock)
                   
                 }
                 )
                 
                 
                 output$value53Vte <- renderValueBox({
                   dataf_facture<-dataf_facture()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53Vte<-sum(dataf_facture$NbFacture)
                   }else{
                     valeur53Vte<-0
                   }
                   valueBox(formatC(valeur53Vte,digits = 0,format ="f",big.mark=' ' ),'Nbre Factures',color = "black")
                 })
                 
                 output$value53VtePoidsNet <- renderValueBox({
                   dataf_facture<-dataf_facture()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53VtePoidsNet<-sum(dataf_facture$PoidsNet)
                   }else{
                     valeur53VtePoidsNet<-0
                   }
                   valueBox(formatC(valeur53VtePoidsNet,digits = 0,format ="f",big.mark=' ' ),'Poids Net(Kg)',color = "black")
                 })
                 
                 output$value53VteNbBalle <- renderValueBox({
                   dataf_facture<-dataf_facture()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53VteNbBalle<-sum(dataf_facture$NbBalle)
                   }else{
                     valeur53VteNbBalle<-0
                   }
                   valueBox(formatC(valeur53VteNbBalle,digits = 0,format ="f",big.mark=' ' ),'Nbre Balles',color = "black")
                 })
                            
                 output$value53VteMontant <- renderValueBox({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53VteMontant<-sum(as.numeric(dataf_facture$montant_tot_fact_def))
                   }else{
                     valeur53VteMontant<-0
                   }
                   valueBox(formatC(valeur53VteMontant,digits = 0,format ="f",big.mark=' ' ),'Montant (Euro)',color = "black")
                 })
                 
                 output$value53NbContrat <- renderValueBox({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53NbContrat<-nrow(as.data.frame(unique(dataf_facture$rf_contrats_sdcc)))
                   }else{
                     valeur53NbContrat<-0
                   }
                   valueBox(formatC(valeur53NbContrat,digits = 0,format ="f",big.mark=' ' ),'Nbre de contrats',color = "green")
                 })
                 
                 
                 output$value53NbClient <- renderValueBox({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53NbClient<-nrow(as.data.frame(unique(dataf_facture$nm_negoci)))
                   }else{
                     valeur53NbClient<-0
                   }
                   valueBox(formatC(valeur53NbClient,digits = 0,format ="f",big.mark=' ' ),'Nbre de clients',color = "green")
                 })
                 
                 output$value53PUMin <- renderValueBox({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53PUMin<-min(as.character(dataf_facture$pu_euro))
                   }else{
                     valeur53PUMin<-0
                   }
                   valueBox(formatC(valeur53PUMin,digits = 4,format ="f",big.mark=' ' ),'PU. Minimum(Euro)',color = "green")
                 })
                 
                 
                 output$value53PUMax <- renderValueBox({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0){
                     valeur53PUMax<-max(as.character(dataf_facture$pu_euro))
                   }else{
                     valeur53PUMax<-0
                   }
                   valueBox(formatC(valeur53PUMax,digits = 4,format ="f",big.mark=' ' ),'PU. Maximum(Euro)',color = "green")
                 })
                 
                 output$TabtauxVente = renderDT({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0)
                   {
                     
                     #dataf_facture<-dataf_facture()[complete.cases(dataf_facture()$montant_tot_fact_def),]
                     dataf_facture<-dataf_facture %>%
                       group_by(typeF) %>%
                       summarize(
                         Nb_balle=sum(nb_balle),
                         Montant=sum(montant_tot_fact_def)
                         
                       )
                     VTotal <-sum(dataf_facture$Montant)
                     dataf_facture$Pourc<-(dataf_facture$Montant/VTotal)*100
                     dataf_facture$Pourc <-formatC(dataf_facture$Pourc,digits = 3,format ="f",big.mark=' ' )
                     
                     #dataf_classement$TonBrut <-formatC(dataf_classement$TonBrut,digits = 2,format ="f",big.mark=' ' )
                     #dataf_facture$TonNet <-formatC(dataf_classement$TonNet,digits = 2,format ="f",big.mark=' ' )
                     names(dataf_facture)<-c("Type","NbBalles","Montant(Euro)","Pourc(%)")
                     datatable(dataf_facture, rownames = FALSE,
                               options=list(
                                 pageLength = 5,
                                 lengthMenu = c(5), 
                                 autoWidth = FALSE)
                     ) 
                   } 
                   
                 })
                 
                 
                 output$TabtauxVenteParClient = renderDT({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0)
                   {
                     
                     #dataf_facture<-dataf_facture()[complete.cases(dataf_facture()$montant_tot_fact_def),]
                     dataf_facture<-dataf_facture %>%
                       group_by(nm_negoci) %>%
                       summarize(
                         Nb_balle=sum(nb_balle),
                         Montant=sum(montant_tot_fact_def)
                         
                       )
                     VTotal <-sum(dataf_facture$Montant)
                     dataf_facture$Pourc<-(dataf_facture$Montant/VTotal)*100
                     dataf_facture$Pourc <-formatC(dataf_facture$Pourc,digits = 3,format ="f",big.mark=' ' )
                     
                     names(dataf_facture)<-c("Client","NbBalles","Montant(Euro)","Pourc(%)")
                     datatable(dataf_facture, rownames = FALSE,
                               options=list(
                                 pageLength = 5,
                                 lengthMenu = c(5), 
                                 autoWidth = FALSE)
                     ) 
                   } 
                   
                 })
                 
                 
                 
                 
                 output$GTauxVenteParType <- renderPlotly({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0)
                   {
                   dataf_facture<-dataf_facture %>%
                     group_by(typeF,li_pays) %>%
                     summarize(
                       Montant=sum(montant_tot_fact_def)/1000
                     )
                   VTotal <-sum(dataf_facture$Montant)
                   dataf_facture$Pourc<-(dataf_facture$Montant/VTotal)*100
                   dataf_facture<-arrange(dataf_facture,-Pourc) 
                   dataf_facture = dataf_facture %>% mutate_if(is.factor,
                                                                     fct_explicit_na,
                                                                     na_level = "NA")
                   
                   dataf_facture$typeF  <- with(dataf_facture, reorder(typeF, -Montant))
                   p<- ggplot(data=dataf_facture, aes(x = reorder(typeF, Montant), y = Montant, fill = li_pays)) + 
                     geom_bar(stat = "identity")+ 
                     coord_flip()+
                     labs(x = ".", 
                          y = "Montant(KEuro)"
                     )#+
                     #theme(axis.title.y=element_blank(),
                     #       axis.text.y=element_blank(),
                     #      axis.ticks.y=element_blank())
                   #theme_minimal()
                   ggplotly(p)
                    
                   }
                   }
                 )
                 
                 
                 output$GTauxVenteParClient <- renderPlotly({
                   dataf_facture<-dataf_factureDef()
                   nb<-nrow(dataf_facture)
                   if(nb!=0)
                   {
                     dataf_facture<-dataf_facture %>%
                       group_by(nm_negoci,li_pays) %>%
                       summarize(
                         Montant=sum(montant_tot_fact_def)/1000
                       )
                     VTotal <-sum(dataf_facture$Montant)
                     dataf_facture$Pourc<-(dataf_facture$Montant/VTotal)*100
                     dataf_facture<-arrange(dataf_facture,-Pourc) 
                     dataf_facture = dataf_facture %>% mutate_if(is.factor,
                                                                 fct_explicit_na,
                                                                 na_level = "NA")
                     
                     dataf_facture$nm_negoci  <- with(dataf_facture, reorder(nm_negoci, -Montant))
                     p<- ggplot(data=dataf_facture, aes(x = reorder(nm_negoci, Montant), y = Montant, fill = li_pays)) + 
                       geom_bar(stat = "identity")+ 
                       coord_flip()+
                       labs(x = ".", 
                            y = "Montant(KEuro)"
                       )
                     ggplotly(p)
                     
                   }
                 }
                 )
                 
                 
                 #----------------- ACHATS FINANCES --------------------------------------------------------------
                 
                 output$value55 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur55<-sum(dataf_Achat$NbCde)
                   }else{
                     valeur55<-0
                   }
                   valueBox(formatC(valeur55,digits = 0,format ="f",big.mark=' ' ),'Nb Total Cdes',color = "black")
                 })
                 output$value56 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur56<-sum(dataf_Achat$NbCdeCFA)
                   }else{
                     valeur56<-0
                   }
                   valueBox(formatC(valeur56,digits = 0,format ="f",big.mark=' ' ),'Cdes Locales',color = "green")
                 })
                 output$value57 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur57<-sum(dataf_Achat$NbCdeEURO+dataf_Achat$NbCdeUSD)
                   }else{
                     valeur57<-0
                   }
                   valueBox(formatC(valeur57,digits = 0,format ="f",big.mark=' ' ),'Cdes Imports',color = "yellow")
                 })
                 output$value58 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur58<-sum(dataf_Achat$MontantCdeCFA)
                   }else{
                     valeur58<-0
                   }
                   valueBox(formatC(valeur58,digits = 0,format ="f",big.mark=' ' ),'Montant Cdes Locales(CFA)',color = "green")
                 })
                 output$value59 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur59<-sum(dataf_Achat$MontantCdeEURO)
                   }else{
                     valeur59<-0
                   }
                   valueBox(formatC(valeur59,digits = 0,format ="f",big.mark=' ' ),'Montant Cdes imports(Euro)',color = "yellow")
                 })
                 output$value60 <- renderValueBox({
                   dataf_Achat<-dataf_Achat()
                   nb<-nrow(dataf_Achat)
                   if(nb!=0){
                     valeur60<-sum(dataf_Achat$MontantCdeUSD)
                   }else{
                     valeur60<-0
                   }
                   valueBox(formatC(valeur60,digits = 0,format ="f",big.mark=' ' ),'Montant Cdes imports(USD)',color = "blue")
                 })
                 
                 output$value61 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur61<-sum(dataf_AchatCumul$NbCde)
                   valueBox(formatC(valeur61,digits = 0,format ="f",big.mark=' ' ),'Total Cdes-Mois Prec.',color = "black")
                 })
                 output$value62 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur62<-sum(dataf_AchatCumul$NbCdeCFA)
                   valueBox(formatC(valeur62,digits = 0,format ="f",big.mark=' ' ),'Locales -Mois Prec.',color = "green")
                 })
                 output$value63 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur63<-sum(dataf_AchatCumul$NbCdeEURO+dataf_AchatCumul$NbCdeUSD)
                   valueBox(formatC(valeur63,digits = 0,format ="f",big.mark=' ' ),'Imports -Mois Prec.',color = "yellow")
                 })
                 output$value64 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur64<-sum(dataf_AchatCumul$MontantCdeCFA)
                   valueBox(formatC(valeur64,digits = 0,format ="f",big.mark=' ' ),'Locales(CFA) -Mois Prec.',color = "green")
                 })
                 output$value65 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur65<-sum(dataf_AchatCumul$MontantCdeEURO)
                   valueBox(formatC(valeur65,digits = 0,format ="f",big.mark=' ' ),'Import(Euro) -Mois Prec.',color = "yellow")
                 })
                 output$value66 <- renderValueBox({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>% filter(Month == VMonthPrec)
                   valeur66<-sum(dataf_AchatCumul$MontantCdeUSD)
                   valueBox(formatC(valeur66,digits = 0,format ="f",big.mark=' ' ),'Import(USD) -Mois Prec.',color = "blue")
                 })
                 
                 
                 
                 output$GEvolutionNbCde <- renderPlotly({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>%
                     group_by(DateOpe) %>%
                     summarize(TotalCdes=sum(NbCde),
                               CdesLocales=sum(NbCdeCFA),
                               CdesImpot_euro=sum(NbCdeEURO),
                               CdesImpot_USD=sum(NbCdeUSD)
                              
                     )
                   
                   
                   if(nrow(dataf_AchatCumul)>0){
                     p<-ggplot(data=dataf_AchatCumul,aes(x = DateOpe)) + 
                       geom_line(aes(y=TotalCdes,colour ="TotalCdes")) + 
                       geom_line(aes(y=CdesLocales,colour ="CdesLocales")) + 
                       geom_line(aes(y=CdesImpot_euro,colour ="CdesImpot_euro")) +
                       geom_line(aes(y=CdesImpot_USD,colour ="CdesImpot_USD")) +
                       scale_colour_manual("", 
                                           breaks = c("TotalCdes", "CdesLocales", "CdesImpot_euro","CdesImpot_USD"),
                                           values = c("black", "blue","yellow","green"))+ 
                       
                       ylab('Nbre Cdes')+xlab('date')
                     
                   }
                 }
                 )
                 
                 
                 output$TabCdeEvolution = renderDT({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>%
                     group_by(MonthName,Month) %>%
                     summarize(TotalCdes=sum(NbCde),
                               CdesLocales=sum(NbCdeCFA),
                               CdesImpot_euro=sum(NbCdeEURO),
                               CdesImpot_USD=sum(NbCdeUSD)
                     )
                   names(dataf_AchatCumul)<-c("Mois","Month","TotalCdes","CdesLocales","CdesImpot_euro","CdesImpot_USD")
                   dataf_AchatCumul <- dataf_AchatCumul[order(dataf_AchatCumul$Month),c("Mois","TotalCdes","CdesLocales","CdesImpot_euro","CdesImpot_USD")]
                   
                   datatable(dataf_AchatCumul, rownames = FALSE,selection = 'single',
                             options=list(
                               dom = 'tp',
                               pageLength = 6,
                               lengthMenu = c(6),
                               autoWidth = FALSE
                             ))
                   
                 })
                 
                 output$GEvolutionCdeMontant <- renderPlotly({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>%
                     group_by(DateOpe) %>%
                     summarize(
                               MontantCFA=sum(MontantCdeCFA),
                               MontantEURO=sum(MontantCdeEURO),
                               MontantUSD=sum(MontantCdeUSD)
                               
                     )
                   
                   
                   if(nrow(dataf_AchatCumul)>0){
                     p<-ggplot(data=dataf_AchatCumul,aes(x = DateOpe)) + 
                       geom_line(aes(y=MontantCFA,colour ="MontantCFA")) + 
                       geom_line(aes(y=MontantEURO,colour ="MontantEURO")) +
                       geom_line(aes(y=MontantUSD,colour ="MontantUSD")) +
                       scale_colour_manual("", 
                                           breaks = c("MontantCFA", "MontantEURO", "MontantUSD"),
                                           values = c("blue","yellow","green"))+ 
                       
                       ylab('Montant')+xlab('date')
                     
                   }
                 }
                 )
                 
                 output$TabMonantCdeEvolution = renderDT({
                   dataf_AchatCumul<-dataf_AchatCumul()
                   dataf_AchatCumul<-dataf_AchatCumul %>%
                     group_by(MonthName,Month) %>%
                     summarize(
                       MontantCFA=sum(MontantCdeCFA),
                       MontantEURO=sum(MontantCdeEURO),
                       MontantUSD=sum(MontantCdeUSD)
                     )
                   
                   dataf_AchatCumul$MontantCFA<- formatC(dataf_AchatCumul$MontantCFA,digits = 0,format ="f",big.mark=' ' )
                   dataf_AchatCumul$MontantEURO<- formatC(dataf_AchatCumul$MontantEURO,digits = 0,format ="f",big.mark=' ' )
                   dataf_AchatCumul$MontantUSD<- formatC(dataf_AchatCumul$MontantUSD,digits = 0,format ="f",big.mark=' ' )
                   
                   names(dataf_AchatCumul)<-c("Mois","Month","MontantCFA","MontantEURO","MontantUSD")
                   dataf_AchatCumul <- dataf_AchatCumul[order(dataf_AchatCumul$Month),c("Mois","MontantCFA","MontantEURO","MontantUSD")]
                   
                   datatable(dataf_AchatCumul, rownames = FALSE,selection = 'single',
                             options=list(
                               dom = 'tp',
                               pageLength = 6,
                               lengthMenu = c(6),
                               autoWidth = FALSE
                             ))
                   
                 })
                 
                 
                 
                 output$GMontantCdeParFsseur <- renderPlotly({
                   dataf_Achat<-dataf_Achat()
                   if(nrow(dataf_Achat)>0){
                     dataf_Achat<-dataf_Achat %>%
                       group_by(NOMFOU) %>%
                       summarize(TotalCde=sum(MontantCdeCFA)/1000000)
                     dataf_Achat<-head(arrange(dataf_Achat,-TotalCde),10) # top 10 des produits
                     dataf_Achat = dataf_Achat %>% mutate_if(is.factor,
                                                                       fct_explicit_na,
                                                                       na_level = "NA")
                     
                     dataf_Achat$NOMFOU  <- with(dataf_Achat, reorder(NOMFOU, -TotalCde))
                     p<- ggplot(data=dataf_Achat, aes(x = reorder(NOMFOU, TotalCde), y = TotalCde, fill = NOMFOU)) + 
                       geom_bar(stat = "identity")+ 
                       coord_flip()+
                       labs(x = "Fournisseur", 
                            y = "MontantCde (Millions CFA)",
                            title = "TOP 10 - Montant en Millions FCFA")+
                       theme(axis.title.y=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks.y=element_blank())
                       #scale_x_discrete(labels = comma)
                     
                     #theme_minimal()
                     ggplotly(p)
                   }
                   
                 }
                 )
                 
                 output$GMontantCdeParDate <- renderPlotly({
                   dataf_Achat<-dataf_Achat()
                   if(nrow(dataf_Achat)>0){
                     dataf_Achat<-dataf_Achat %>%
                       group_by(DateOpe) %>%
                       summarize(TotalCde=sum(MontantCdeCFA)/1000000)
                     dataf_Achat<-head(arrange(dataf_Achat,-TotalCde),10) # top 10 des produits
                     dataf_Achat = dataf_Achat %>% mutate_if(is.factor,
                                                             fct_explicit_na,
                                                             na_level = "NA")
                     
                     dataf_Achat$DateOpe  <- with(dataf_Achat, reorder(DateOpe, -TotalCde))
                     p<- ggplot(data=dataf_Achat, aes(x = reorder(DateOpe, TotalCde), y = TotalCde, fill = DateOpe)) + 
                       geom_bar(stat = "identity")+ 
                       coord_flip()+
                       labs(x = "DateCde", 
                            y = "MontantCde (Millions CFA)",
                            title = "TOP 10 - Montant en Millions FCFA")+
                       theme(axis.title.y=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks.y=element_blank())
                     #scale_x_discrete(labels = comma)
                     
                     #theme_minimal()
                     ggplotly(p)
                   }
                   
                 }
                 )
                 
 
                 
                 #============= Parametrage des utilisateurs========================================================================#    
                 dataf_Utilisateur= reactive({
                   input$updateUtilisateurP
                   input$deleteUtilisateurP
                   input$insertUtilisateurP
                   get_dataUtilisateur()
                 }
                 )
                 
                 
                 output$Utilisateur = renderDT({
                   updateTextInput(session,"CodeUtilisateurP",value="")
                   updateTextInput(session,"NomUtilisateurP",value="")
                   dataf_Utilisateur<-dataf_Utilisateur()
                   datatable(dataf_Utilisateur, rownames = FALSE,selection = 'single',
                             options=list(
                               pageLength = 10,
                               lengthMenu = c(10),
                               columnDefs = list(list(visible=FALSE, targets=c(2)))
                             )
                   )  
                 }
                 )
                 outputOptions(output,'Utilisateur', priority=-100)
                 
                 
                 observe({
                   if(!is.null(input$Utilisateur_rows_selected)){
                     updatevalue=dataf_Utilisateur()[input$Utilisateur_rows_selected,]
                     updateTextInput(session,"CodeUtilisateurP",value=updatevalue$CodeUtilisateur)
                     updateTextInput(session,"CodeProfil",value=updatevalue$ProfilUtilisateur)
                     updateTextInput(session,"NomUtilisateurP",value=updatevalue$NomUtilisateur)
                     updateTextInput(session,"MotPasseUtilisateurP",value=updatevalue$MotPasse)
                     
                   }
                 }
                 )
                 
                 
                 observeEvent(input$CodeProfil,{
                   VCodeProfil<<-input$CodeProfil
                 }
                 )
                 observeEvent(input$CodeUtilisateurP,{
                   VCodeUtilisateurP<<-input$CodeUtilisateurP
                 }
                 )
                 observeEvent(input$NomUtilisateurP,{
                   VNomUtilisateurP<<-input$NomUtilisateurP
                 }
                 )
                 observeEvent(input$MotPasseUtilisateurP,{
                   VMotPasse<<-input$MotPasseUtilisateurP
                 }
                 )
                 
                 observeEvent(input$updateUtilisateurP,{
                   req="update Utilisateur set NomUtilisateur= cast(? as varchar), 
                        MotPasse=cast(? as varchar),
                        ProfilUtilisateur=cast(? as varchar)
                        where CodeUtilisateur like ?
                      "
                   myvalue=data.frame(input$NomUtilisateurP,input$MotPasseUtilisateurP,input$CodeProfil,input$CodeUtilisateurP)
                   sqlExecute(cn, req, myvalue)
                   updateTextInput(session,"CodeProfil",value="")
                   updateTextInput(session,"NomSocieteP",value="")
                   alert("Modification effectuee!")
                   
                 },priority=200
                 )
                 
                 observeEvent(input$insertUtilisateurP,{
                   
                   nbUtilisateur<-0
                   reqRechUtilisateur<-"select * from Utilisateur where CodeUtilisateur like cast(? as varchar)"
                   myvalue<-data.frame(input$CodeUtilisateurP)
                   ListUtilisateur<-sqlExecute(cn, reqRechUtilisateur, myvalue,fetch=TRUE)
                   nbUtilisateur<-nrow(ListUtilisateur)
                   
                   if(VCodeProfil!="" 
                      & VCodeUtilisateurP!=""
                      & VNomUtilisateurP!=""
                      & VMotPasse!=""
                      & nbUtilisateur==0)
                     
                   {
                     req<-"insert into Utilisateur(CodeSociete, CodeUtilisateur, NomUtilisateur, MotPasse,ProfilUtilisateur) values(?,?,?,?,?)"
                     myvalue=data.frame("Sod",input$CodeUtilisateurP,input$NomUtilisateurP,input$MotPasseUtilisateurP,input$CodeProfil)
                     sqlExecute(cn, req, myvalue)
                     updateTextInput(session,"CodeProfil",value="")
                     updateTextInput(session,"CodeUtilisateurP",value="")  
                     updateTextInput(session,"NomUtilisateurP",value="") 
                     updateTextInput(session,"MotPasse",value="") 
                     alert("Creation effectuee")
                     
                     #-- Actualisation liste des utilisateurs
                     reqUtilisateur<-"  select '' CodeUtilisateur, '' NomUtilisateur
                   UNION ALL
                   select CodeUtilisateur,NomUtilisateur from Utilisateur"
                     
                     VUtilisateur<-sqlExecute(cn, reqUtilisateur,fetch=TRUE)
                     choicesUtilisateur <<- setNames(VUtilisateur$CodeUtilisateur,VUtilisateur$NomUtilisateur)
                     updateSelectInput(session,'CodeUtilisateuP2',choices=choicesUtilisateur)
                     
                   }else{
                     if(nbUtilisateur!=0){
                       alert("Cet Utilisateur existe deja")
                     } else{
                       if(is.null(input$CodeProfil) | input$CodeProfil==""){
                         alert("Societe non saisi!")
                       }else{
                         if(is.null(input$CodeUtilisateurP) | input$CodeUtilisateurP==""){
                           alert("Code utilisateur non saisi!")
                         }else{
                           if(is.null(input$NomUtilisateurP) | input$NomUtilisateurP==""){
                             alert("Nom utilisateur non saisi!")
                           }else{
                             if(is.null(input$MotPasseUtilisateurP) | input$MotPasseUtilisateurP==""){
                               alert("Mot de passe non saisie!")
                             }
                           }
                         }
                       }
                     }
                   }
                   
                   
                 },priority=200
                 )
                 
                 #=========================================================================================================
                 
                 
                                 
                 
                  #----------------- EVACUATION --------------------------------------------------------------
                
                output$GDelaiMoy <- renderPlotly({
                    dataf_evacuation<-dataf_evacuation()
                    dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$delai),]
                    dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine)
                    dataf_evacuation<-dataf_evacuation %>%
                        group_by(DateOpe) %>%
                        summarize(delaiMoy=mean(delai))
                    
                    if(nrow(dataf_evacuation)>0){
                        p<-ggplot(data=dataf_evacuation, aes(x = DateOpe, y = delaiMoy)) + 
                            geom_line(size = 0.5       
                            ) + 
                            labs(x = "DateProduction", 
                                 y = "Evolution Delai(Jours))",
                                 title = "Delai Moyen Evacuation en JOURS")+  
                            theme_minimal()
                        ggplotly(p)
                    }
                }
                )
                
                
                output$GCorrelation <- renderPlotly({
                  dataf_evacuation<-dataf_evacuation()
                  dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$delai),]
                  dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine)
                  dataf_evacuation<-dataf_evacuation %>%
                    group_by(DateProduction,Transporteur) %>%
                    summarize(delai=mean(delai),
                              NbBalles=sum(NbBalles)
                              )
                  
                  if(nrow(dataf_evacuation)>0){
                    p<-ggplot(dataf_evacuation, aes(x=NbBalles, y=delai)) + 
                      geom_point() + ggtitle("NbBalles vs delai") + 
                      geom_smooth(method=lm, se=FALSE)+
                    
                      labs(x = "Nombre de Balles", 
                           y = "Delai (Jours)",
                           title = "Correlation Delai  Vs Nb Balles")
                    ggplotly(p)
                  }
                }
                )
                
                output$GEvacuationHist <- renderPlot({
                  dataf_evacuation<-dataf_evacuation()
                  dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$delai),]
                  dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine)
                  
                  if(nrow(dataf_evacuation)>0){
                    hist(dataf_evacuation$delai, 
                         col = "blue",
                         main = "Frequence Delai Evacuation",
                         xlab = "Delai Evacuation (Jours)",
                         ylab = "Frequence")
                    
                  }
                }
                )
                
                
                #output$value5 <- renderValueBox({
                #    dataf_evacuation<-dataf_evacuation()
                #    dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$delai),]
                #    dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                #    Valeur5 <- mean(dataf_evacuation$delai)
                #    valueBox(formatC(Valeur5,digits = 0,format ="f",big.mark=',' ),'Delai Moyen Evacuation (Jours)',color = "purple")
                #})
                #output$value51 <- renderValueBox({
                #    dataf_evacuation<-dataf_evacuation()
                #    dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$delai),]
                #    dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                #    Valeur6 <- sum(dataf_evacuation$NbBalles)
                #    valueBox(formatC(Valeur6,digits = 0,format ="f",big.mark=',' ),'Nb de Balles Evacues',color = "yellow")
                #})
                
                
                output$TabBallesEvacuation = renderDT({
                    dataf_evacuation<-dataf_evacuation()
                    #dataf_evacuation<-dataf_evacuation()[complete.cases(dataf_evacuation()$Ecart_Poids),]
                    dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                    dataf_evacuation<-head(dataf_evacuation[order(dataf_evacuation$DateProduction),],5)
                    dataf_evacuation<-dataf_evacuation %>% 
                        rename(
                            DateProduction = DateOpe,
                            Delai_Evacuation=delai
                         )
                    dataf_evacuation<-dataf_evacuation[,c("DateProduction","NbBalles","PoidsBrut","PoidsNet","Ecart_Poids","Delai_Evacuation")]
                    datatable(dataf_evacuation, rownames = FALSE) 
                })
                
                output$TabTransporteur = renderDT({
                  dataf_evacuation<-dataf_evacuation()
                  dataf_evacuation<-dataf_evacuation %>% filter(Periode == VYear & IdUsine==VIdUsine & Month==VMonth)
                
                  dataf_evacuation<-dataf_evacuation %>%
                    group_by(Usine,Transporteur) %>%
                    summarize(delaiMoy=mean(delai),
                              NbBalles=sum(NbBalles)
                    )
                  dataf_evacuation$delaiMoy <-formatC(dataf_evacuation$delaiMoy,digits = 1,format ="f",big.mark=',' )
                  dataf_evacuation<-dataf_evacuation[,c("Transporteur","delaiMoy","NbBalles")]
                  datatable(dataf_evacuation, rownames = FALSE,
                            options=list(autoWidth = FALSE,
                                         pageLength = 8,
                                         lengthMenu = c(8)
                                         ) 
                            )
                })
                
                
                
                appendTab(inputId = "tabselected",
                          
                          tabPanel("PROD.-EGRENAGE", frow1,frow4,frow3,frow2
                                   
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("PROD.-DELINTAGE",
                                   div(style = "color:maroon;font-weight: 500",
                                   tags$h3(" "),
                                   )
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("PROD.-CLASSEMENT FIBRE",frow6,frow8,frow7,frow9,frow10
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("PROD.-MAINTENANCE",
                                   frow100,
                                   frow102,
                                   tags$br(),
                                   h2("Analyse des pannes, Maintenance & Arrêts "),
                                   frow33,
                                   frow34,
                                   frow35
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("RESSOURCES H.",
                                   frow11,frow12b,frow15b,frow15c,frow14,frow15,frow29,
                                   
                                   tags$br(),
                                   h3("INFORMATIONS SUR LES DEPARTS"),
                                   frow17,
                                   frow18,
                                   frow18b,
                                   tags$br(),
                                   h3("INFORMATIONS SUR LES RECRUTEMENTS"),
                                   frow19,
                                   frow20,
                                   tags$br(),
                                   h3("INFORMATIONS SUR LES MISIONS"),
                                   frow30,
                                   frow31,
                                   frow32
                                   
                                   
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("VENTES & STOCKS",
                                   h2("FACTURATION FIBRES"),
                                   frow21,
                                   frow21c,
                                   frow21a,
                                   frow21b,
                                   h2("STOCK HUILES"),
                                   frow22
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("ACHATS & FINANCES",
                                   frow24,
                                   frow25,
                                   frow26,
                                   frow27,
                                   frow28
                                   
                          ) # closes tabPanel,
                )
                appendTab(inputId = "tabselected",
                          tabPanel("SYNTHESE",
                                   h2("SYNTHESE TOTALE"),
                                   h3("1- Production Usines"),
                                   frow1Synhese,
                                   h3("2- Maintenance"),
                                   
                                   
                          ) # closes tabPanel,
                )
                
                appendTab(inputId = "tabselected",
                          
                          tabPanel("HUILERIES", frowH1, frowH2, frowH3, frowH4, frowH5
                                   
                          ) # closes tabPanel,
                )
                
                appendTab(inputId = "tabselected",
                          
                          tabPanel("INFORMATIQUE", PremiereLigne_Informatique, DeuxiemeLigne_Informatique
                                   
                          ) # closes tabPanel,
                )
                
                appendTab(inputId = "tabselected",
                          tabPanel("PARAMETRAGE",
                                   tabsetPanel(id="Consultation",
                                               tabPanel(tags$h4("Objectifs"),
                                                        tabsetPanel(id="Consultationx",
                                                                    tabPanel(tags$h4("Production usines")),
                                                                    tabPanel(tags$h4("Maintenance")),
                                                                    tabPanel(tags$h4("Ventes")),
                                                                    tabPanel(tags$h4("Achats")),
                                                                    tabPanel(tags$h4("Ressources Humaines"))
                                                        )
                                                        
                                                        ),
                                               tabPanel(tags$h4("Utilisateurs"),
                                                        
                                                        tags$h3("   GESTION DES UTILISATEURS"),
                                                        fluidRow(div(style="height:500px; width:1540px; border: 2px solid gray",
                                                                     
                                                                     column(3,div(style = "height:400px;background-color: white;", 
                                                                                  textInput("CodeUtilisateurP", "Code Utilisateur"),
                                                                                  textInput("NomUtilisateurP", "Nom Utilisateur"),
                                                                                  passwordInput("MotPasseUtilisateurP", "Mot de passe"),
                                                                                  selectInput('CodeProfil',"Profil",choices = c("DFC","DRH","PRODUCTION","ADMIN")),
                                                                                  actionButton("updateUtilisateurP", "Modification", class = "btn-primary"),
                                                                                  disabled(
                                                                                    actionButton("deleteUtilisateurP", "Suppression")
                                                                                  ),
                                                                                  actionButton("insertUtilisateurP", "Ajout")
                                                                     )
                                                                     ),
                                                                     column(6,div(style = "height:400px;background-color: white;",
                                                                                  #tags$head(
                                                                                  # tags$style(HTML(css))
                                                                                  #),
                                                                                  DTOutput("Utilisateur")
                                                                     )
                                                                     )
                                                        )
                                                        )
                                                        
                                                        )
                                   ),
                                   
                                   
                                   
                          ) # closes tabPanel,
                )
                
                removeTab(inputId = "tabselected",
                          target = "1")
                
                if(user$ProfilUtilisateur=="DRH"){
                  hideTab(inputId = "tabselected", target = "ACHATS & FINANCES")
                  hideTab(inputId = "tabselected", target = "HUILERIE-STOCK&VENTES")
                  hideTab(inputId = "tabselected", target = "PROD.-MAINTENANCE")
                  hideTab(inputId = "tabselected", target = "PROD.-EGRENAGE")
                  hideTab(inputId = "tabselected", target = "PROD.-CLASSEMENT FIBRE")
                  hideTab(inputId = "tabselected", target = "GESTION UTILISATEURS")
                  hideTab(inputId = "tabselected", target = "PROD.-DELINTAGE")
                }
                if(user$ProfilUtilisateur=="DFC"){
                  hideTab(inputId = "tabselected", target = "RESSOURCES HUMAINES")
                  hideTab(inputId = "tabselected", target = "HUILERIE-STOCK&VENTES")
                  hideTab(inputId = "tabselected", target = "PROD.-MAINTENANCE")
                  hideTab(inputId = "tabselected", target = "PROD.-EGRENAGE")
                  hideTab(inputId = "tabselected", target = "PROD.-CLASSEMENT FIBRE")
                  hideTab(inputId = "tabselected", target = "GESTION UTILISATEURS")
                  hideTab(inputId = "tabselected", target = "PROD.-DELINTAGE")
                }
                if(user$ProfilUtilisateur=="PRODUCTION"){
                  hideTab(inputId = "tabselected", target = "RESSOURCES HUMAINES")
                  hideTab(inputId = "tabselected", target = "ACHATS & FINANCES")
                  hideTab(inputId = "tabselected", target = "GESTION UTILISATEURS")
                }
                
                
            
                } else { # username correct, password wrong
                
            } # closes if-clause
            
        } 
      else { #  username name wrong or more than 3 log-in failures 
           alert("Nom utilisateur ou mot de passe incorrect!")
        } # closes second if-clause
        
    }) # closes observeEvent
    
    observeEvent(input$ScreenDownload,{
      shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
      screenshot(scale = 0.7)
      
    }
    )
}
#shinyApp(ui, Server, onStop(function() {odbcClose(cn)}))
shinyApp(ui, Server, onStop(function() {odbcCloseAll()}))



