#------------------------------------------------------------------------------------------------
#----------------------------------||--PRIM'S ALGORITHM--||--------------------------------------
#------------------------------------------------------------------------------------------------


#---------------------------------------- OPTIONS -----
options(expressions=1000)

#---------------------------------------- FUNCTIONS -----

#Seleciona o melhor caminho da franja e adiciona na �rvore m�nima
pickBestOnFranja <- function(){
    #ordenando franja pelo menor peso
    franja <<- franja[order(franja[,3]),]
    
    #Pega o menor caminho da franja
    bestNode <- franja[1,]
    
    #Se os v�rtices desse caminho j� foram visitados este caminho n�o � v�lido e deve ser excluido da franja para n�o formar ciclo
    if(bestNode[,1] %in% visited & bestNode[,2] %in% visited){
        franja <<- franja[-1,]
        #Depois de elimitar o caminho da franja executar a fun��o novamente para recome�ar o processo
        pickBestOnFranja()
    }else{
        #Se o caminho selecionado da franja � um caminho v�lido adicionar este caminho na �rvore m�nima
        am[nrow(am)+1,1] <<- as.character(bestNode[1,1])
        am[nrow(am),2] <<- as.character(bestNode[1,2])
        am[nrow(am),3] <<- bestNode[1,3]
    }
}


#Retorna o pr�ximo v�rtice que deve ter a sua franja avaliada
findAvailNode <- function(){
    #Vertice candidato a ser avaliado
    nodeCandidate <- NA
    #Caso a �rvore m�nima esteja vazia(primeira execu��o), pegar um v�rtice qualquer
    if(nrow(am) == 0){
        i <- 1
        while(is.na(nodeCandidate) & i <= length(vertices)){
            if(!vertices[i] %in% visited){
                visited <<- c(visited, as.character(vertices[i]))
                nodeCandidate <- vertices[i]
            } 
            
            i <- i + 1
        }
    } else{
        #Se n�o � a primeira execu��o seleciona o �ltimo v�rtice "destino"
        nodeCandidate <- am[nrow(am),2]
        #Caso o v�rtice candidato j� tenha sido visitado ele n�o � utilizado, para n�o formar ciclos
        if(nodeCandidate %in% visited)
            nodeCandidate <- am[nrow(am),1]    
        #Adiciona o v�rice a lista de v�rtices visitados
        visited <<- c(visited, as.character(nodeCandidate))
    }
    
    #Retornando o v�rtice encontrado
    as.character(nodeCandidate)
}





#---------------------------------------- GRAPHS -----

# FIG 1.png
#    a <- data.frame(
#        v1 = c("a","a","b","d"),
#        v2 = c("b","d","d","c"),
#        p  = c(2,1,2,3),
#        stringsAsFactors=FALSE
#        )


# FIG 2.png
#a <- data.frame(
#    v1 = c("a","a","a","b","b","c","c","d","e"),
#    v2 = c("b","d","e","d","e","e","f","e","f"),
#    p  = c(1,4,3,4,2,4,5,4,7),
#    stringsAsFactors=FALSE
#)


# FIG 3.png
#a <- data.frame(
#    v1 = c("a","a","a","b","b","c","c","c","d","e","f"),
#    v2 = c("b","c","d","c","e","d","f","e","f","f","g"),
#    p  = c(2,3,3,4,3,5,6,1,7,8,9),
#    stringsAsFactors=FALSE
#)


# FIG 4.png
#a <- data.frame(
#    v1 = c("a","a","b","b","c","d","e"),
#    v2 = c("b","c","d","e","d","f","f"),
#    p  = c(1,7,5,2,6,4,3),
#    stringsAsFactors=FALSE
#)


# FIG 5.png
#a <- data.frame(
#    v1 = c("a","a","b","b","c","d","d","e","e","f","g"),
#    v2 = c("b","g","c","d","h","e","f","f","g","h","h"),
#    p  = c(4,5,3,2,4,1,1,1,2,2,1),
#    stringsAsFactors=FALSE
#)


# FIG 6.png
#a <- data.frame(
#    v1 = c("0","0","1","1","2","2","2","3","3","4","5","6","6","7"),
#    v2 = c("1","7","2","7","3","8","5","4","5","5","6","7","8","8"),
#    p  = c(4,8,8,11,7,2,4,9,14,10,2,1,6,7),
#    stringsAsFactors=FALSE
#)


# FIG 7.png
#a <- data.frame(
#    v1 = c("a","a","b","b","b","c","d","d","e","e","f"),
#    v2 = c("b","d","c","d","e","e","e","f","f","g","g"),
#    p  = c(7,5,8,9,7,5,15,6,8,9,11),
#    stringsAsFactors=FALSE
#)


# FIG 8.png
#a <- data.frame(
#    v1 = c("1","1","1","2","2","3","3","3","5"),
#    v2 = c("2","3","4","3","6","4","5","6","6"),
#    p  = c(3,7,3,2,9,1,3,6,3),
#    stringsAsFactors=FALSE
#)


# FIG 9.png
#a <- data.frame(
#    v1 = c("0","0","2","2","2","1","1","3","3","5","4","4"),
#    v2 = c("2","3","3","4","1","4","6","4","5","6","5","6"),
#    p  = c(5,8,10,3,16,30,26,2,18,4,12,14),
#    stringsAsFactors=FALSE
#)


# FIG 10.png
#a <- data.frame(
#    v1 = c("1","1","2","2","3","3","3","4","4","5"),
#    v2 = c("3","4","3","4","4","5","6","5","6","6"),
#    p  = c(9,7,5,4,4,1,7,2,3,5),
#    stringsAsFactors=FALSE
#)


# FIG 11.png
#a <- data.frame(
#    v1 = c("a","a","b","b","c","c","c","c","d","d","d","e","f","g"),
#    v2 = c("b","h","c","h","d","f","g","h","e","f","g","f","g","h"),
#    p  = c(7,3,6,8,9,2,6,5,3,3,7,5,5,4),
#    stringsAsFactors=FALSE
#)


# FIG 12.png
#a <- data.frame(
#    v1 = c("1","1","1","1","2","2","3","4"),
#    v2 = c("2","5","3","4","3","4","4","5"),
#    p  = c(50,10,30,100,5,20,50,10),
#    stringsAsFactors=FALSE
#)


# FIG 13.png
#a <- data.frame(
#    v1 = c("a","a","a","b","b","b","c","c","d","d","e","e","e","f","f","g","g","h"),
#    v2 = c("b","c","d","c","d","e","e","f","e","g","f","g","h","h","i","h","i","i"),
#    p  = c(4,6,13,2,9,10,7,2,1,2,5,1,4,1,3,3,1,2),
#    stringsAsFactors=FALSE
#)


# FIG 14.png
#a <- data.frame(
#    v1 = c("a","b","b","b","b","c","c","d","d","d","e","f","h"),
#    v2 = c("b","c","g","h","i","f","g","e","f","g","f","g","i"),
#    p  = c(2,3,1,4,5,6,3,6,5,8,4,3,2),
#    stringsAsFactors=FALSE
#)


# FIG 15.png
#a <- data.frame(
#    v1 = c("a","a","a","a","b","c","c","d"),
#    v2 = c("b","c","d","e","d","d","e","e"),
#    p  = c(3,8,5,5,6,1,2,2),
#    stringsAsFactors=FALSE
#)


# FIG 16.png
#a <- data.frame(
#    v1 = c("a","a","a","b","c","d","d","e","e","f","h"),
#    v2 = c("b","c","d","d","d","e","f","f","h","g","i"),
#    p  = c(1,2,5,3,4,7,6,8,10,9,11),
#    stringsAsFactors=FALSE
#)


# FIG 17.png
a <- data.frame(
    v1 = c("a","a","a","a","b","b","b","c","c","d","e"),
    v2 = c("b","d","e","f","c","d","e","d","e","e","f"),
    p  = c(5,8,3,5,4,5,3,3,5,3,7),
    stringsAsFactors=FALSE
)




#---------------------------------------- PROGRAMA -----

    #---------------------------------------- VARIAVEIS -----

    ### Lembrando que o grafo j� foi declarado ali em cima, na variavel "a".

    #Lista com todos os v�rtices do grafo
    vertices <- sort(unique(c(unique(a[,1]),unique(a[,2]))))
    
    #Lista que armazena os v�rtices j� adicionados(Para ficar mais f�cil de verificar os ciclos)
    visited <- character()
    
    #�rvore minima
        am <- data.frame(
            v1 = character(),
            v2 = character(),
            p  = numeric(),
            stringsAsFactors=FALSE
            )
    #franjas
        franja <- data.frame(
            v1 = character(),
            v2 = character(),
            p  = numeric(),
            stringsAsFactors=FALSE
            ) 

    #---------------------------------------- C�DIGO -----

    #Enquanto a franja tiver menos ocorr�ncias do que o n�mero de v�rtices-1, procure por um n� para a arvore m�nima
    while(nrow(am) < length(vertices) - 1){
        #pegando um n� n�o visitado para avaliar a sua franja
        inspectedNode <- findAvailNode()
        
        #Buscando a franja do n� selecionado e adicionando no conjunto de franjas
        auxfranja <- subset(a, v1 == inspectedNode | v2 == inspectedNode)
        franja <- rbind(franja, data.frame(v1=auxfranja[,1], v2=auxfranja[,2], p=auxfranja[,3]))
        
        #selecionando melhor n� da franja e adicionando na �rvore m�nima
        pickBestOnFranja()
    }

























