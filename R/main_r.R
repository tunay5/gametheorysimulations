#' Prisoners' Dilemma
#'
#' @param player_1
#' @param player_2
#' @param player_3
#'
#' @return
#' @export
#'
#' @examples


func_1 <- function(player_1=0,player_2=0,player_3=0){
  if((!any(list(player_1) %in% c(1:3)) || !any(list(player_2) %in% c(1:3))) || (!any(list(player_3) %in% c(1:3,0)))){
    print("Please Include Strategies")

  }else{
    coop_1 <- 0
    coop_2 <- 0

    dev_1 <- 0
    dev_2 <- 0

    tit_tat <- 0
    tit_tat_2 <- 0

    list_1 <- c(player_1, player_2, player_3)

    list_1 <- list_1[list_1!=0]


    if(length(list_1)==3){

      for (i in 1:1000) {
        smpl <- sample(list_1, replace = TRUE,size = length(list_1)-1, prob = rep(1/length(list_1),length(list_1)))

        if(smpl[1]==1 & smpl[2]==1){
          assign("coop_1", c(get("coop_1"),7,7))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1"),7))))
        }

        if(smpl[1]==2 & smpl[2]==2){
          assign("dev_1", c(get("dev_1"),2,2))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1"),2))))
        }

        if(smpl[1]==1 & smpl[2]==2){
          assign("coop_1", c(get("coop_1"),0))
          assign("dev_1", c(get("dev_1"),8))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
        }

        if(smpl[1]==2 & smpl[2]==1){
          assign("coop_1", c(get("coop_1"),0))
          assign("dev_1", c(get("dev_1"),8))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
        }

        if(smpl[1]==3 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("tit_tat", c(get("tit_tat"),7,7))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
          }
          else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("tit_tat", c(get("tit_tat"),7,7))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
            }else{
              assign("tit_tat", c(get("tit_tat"),2,2))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),2))))
            }
          }
        }

        if(smpl[1]==3 & smpl[2]==1 || smpl[1]==1 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("coop_1", c(get("coop_1"),7))
            assign("tit_tat", c(get("tit_tat"),7))
            assign("coop_2", c(get("coop_2"),mean(c(get("coop_1"),7))))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
          }else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("tit_tat", c(get("tit_tat"),7))
              assign("coop_1", c(get("coop_1"),7))
              assign("coop_2", c(get("coop_2"),mean(c(get("coop_1"),7))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
            }else{
              assign("tit_tat", c(get("tit_tat"),8))
              assign("coop_1", c(get("coop_1"),0))
              assign("coop_2", c(get("coop_2"),mean(c(get("coop_1"),0))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),8))))
            }
          }
        }

        if(smpl[1]==3 & smpl[2]==2 || smpl[1]==2 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("dev_1", c(get("dev_1"),8))
            assign("tit_tat", c(get("tit_tat"),0))
            assign("dev_2", c(get("dev_2"),mean(c(get("dev_1"),8))))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),0))))
          }else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("dev_1", c(get("dev_1"),8))
              assign("tit_tat", c(get("tit_tat"),0))
              assign("dev_2", c(get("dev_2"),mean(c(get("dev_1"),8))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),0))))
            }else{
              assign("dev_1", c(get("dev_1"),2))
              assign("tit_tat", c(get("tit_tat"),2))
              assign("dev_2", c(get("dev_2"),mean(c(get("dev_1"),2))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),2))))
            }
          }
        }
        vectors_list <- list(coop_2,dev_2,tit_tat_2)

        colors <- rainbow(length(vectors_list))

        par(mar=c(4.1, 4.1, 3.1, 6.1), xpd=TRUE)


        plot(NULL, xlim = c(1, max(sapply(vectors_list, length))), ylim = c(min(unlist(vectors_list)), max(unlist(vectors_list))), xlab = "Time", ylab = "Average", main = "Prisoners' Dilemma")


        for (i in seq_along(vectors_list)) {
          lines(vectors_list[[i]], col = colors[i])

        }

        legend('topright',legend = c("Cooperate", "Deviate", "Tit for Tat"),col=colors, lty=1, cex=0.8,inset=c(-0.47,0),title="Strategy")

        Sys.sleep(0.7)

      }
    }else{
      for(i in 1:1000){
        smpl <- sample(list_1, size = length(list_1), replace = TRUE, prob = c(1/2,1/2))

        if(smpl[1]==1 & smpl[2]==1){
          assign("coop_1", c(get("coop_1"),7,7))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1"),7))))
        }

        if(smpl[1]==2 & smpl[2]==2){
          assign("dev_1", c(get("dev_1"),2,2))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1"),2))))
        }

        if(smpl[1]==1 & smpl[2]==2){
          assign("coop_1", c(get("coop_1"),0))
          assign("dev_1", c(get("dev_1"),8))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
        }

        if(smpl[1]==2 & smpl[2]==1){
          assign("coop_1", c(get("coop_1"),0))
          assign("dev_1", c(get("dev_1"),8))
          assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
          assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
        }

        if(smpl[1]==3 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("tit_tat", c(get("tit_tat"),7,7))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
          }
          else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("tit_tat", c(get("tit_tat"),7,7))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),7))))
            }else{
              assign("tit_tat", c(get("tit_tat"),2,2))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat"),2))))
            }
          }
        }

        if(smpl[1]==3 & smpl[2]==1 || smpl[1]==1 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("coop_1", c(get("coop_1"),7))
            assign("tit_tat", c(get("tit_tat"),7))
            assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
          }else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("tit_tat", c(get("tit_tat"),7))
              assign("coop_1", c(get("coop_1"),7))
              assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
            }else{
              assign("tit_tat", c(get("tit_tat"),8))
              assign("coop_1", c(get("coop_1"),0))
              assign("coop_2", c(get("coop_2"),mean(c(get("coop_1")))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
            }
          }
        }

        if(smpl[1]==3 & smpl[2]==2 || smpl[1]==2 & smpl[2]==3){
          if(length(tit_tat)==1){
            assign("dev_1", c(get("dev_1"),8))
            assign("tit_tat", c(get("tit_tat"),0))
            assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
            assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
          }else{
            if(any(c(5,7,8) %in% tit_tat[length(tit_tat)])){
              assign("dev_1", c(get("dev_1"),8))
              assign("tit_tat", c(get("tit_tat"),0))
              assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
            }else{
              assign("dev_1", c(get("dev_1"),2))
              assign("tit_tat", c(get("tit_tat"),2))
              assign("dev_2", c(get("dev_2"),mean(c(get("dev_1")))))
              assign("tit_tat_2", c(get("tit_tat_2"),mean(c(get("tit_tat")))))
            }
          }
        }

        vectors_list <- list(coop_2,dev_2,tit_tat_2)

        colors <- rainbow(length(vectors_list))

        par(mar=c(4.1, 4.1, 3.1, 6.1), xpd=TRUE)


        plot(NULL, xlim = c(1, max(sapply(vectors_list, length))), ylim = c(min(unlist(vectors_list)), max(unlist(vectors_list))), xlab = "Time", ylab = "Average", main = "Prisoners' Dilemma")


        for (i in seq_along(vectors_list)) {
          lines(vectors_list[[i]], col = colors[i])

        }

        legend('topright',legend = c("Cooperate", "Deviate", "Tit for Tat"),col=colors, lty=1, cex=0.8,inset=c(-0.47,0),title="Strategy")

        Sys.sleep(0.6)

      }

    }
  }
}

