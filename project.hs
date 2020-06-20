
import DataFile





createEmptyFreqList []=[]
createEmptyFreqList (h:t)=(h,[]):createEmptyFreqList t
		
-- returns a new list with s removed from every position of it 
removefromlist s []=[]
removefromlist s (h:t)=if s==h then removefromlist s t else h:removefromlist s t

-- return true if s occrs in the passed list
occrs s []=False
occrs s (h:t)= if s==h then True else occrs s t


getAllUsersStats purchaseHist=getAllUsersStats1 purchaseHist items


getAllUsersStats1 [] i=[]
getAllUsersStats1 ((name,listofpurchases):t) items=(name,(gomakelist listofpurchases items)):getAllUsersStats1 t items

-- go make list makes the list next to the name of the user
gomakelist l []=[]
gomakelist l (h:t)=(h, (gocount (gofind l h []))):gomakelist l t

--takes a list of lists and finds the lists where item occurs, but the returned list doesnt contain the item being searched for
gofind [] h acc=removefromlist h acc
gofind (h:t) item acc=if occrs item h then gofind t item (acc++h) else gofind t item acc  


gocount []=[]
gocount (h:t)=(h,((length (h:t))-(length (removefromlist h t)))):gocount (removefromlist h t)


purchasesIntersection input1 []=[]
purchasesIntersection input1 ((name,userlist):t)=(purchasesIntersection1 input1 userlist):purchasesIntersection input1 t

purchasesIntersection1 [] []=[]
purchasesIntersection1 ((item,list):t) ((item1,list1):t1)=if (length list)==0 || (length list1)==0 then purchasesIntersection1 t t1 else ((item,(gocount ((go_fok_the_list list)++(go_fok_the_list list1))))):purchasesIntersection1 t t1


--go fok the list takes a list of pairs of (item,int) and returns a list of all items be3adadhom then goconount betgama3hom tani 
go_fok_the_list []= []
go_fok_the_list ((item,num):t)=(go_fok_item_with_num item [] num)++(go_fok_the_list t)

go_fok_item_with_num item acc 0=acc
go_fok_item_with_num item acc num=go_fok_item_with_num item (acc++[item]) (num-1)

                           
freqListItems name=findname name (getAllUsersStats purchasesHistory)

--findname finds the list of the required user
findname name []=[]
findname name ((user,list):t)=if name==user then gocount (go_merge_and_fok [] list) else findname name t


--merges all the lists of the user and betfok el pairs be3adadhom
go_merge_and_fok acc []=acc
go_merge_and_fok acc ((name,list):t)=go_merge_and_fok (acc++(go_fok_the_list list)) t

--logic: we first find the list of lists ofthe required user (so we need to send the purchases history)then gofind all the items in the cart then gocount

freqListCart name cart=(freqListCar name cart purchasesHistory)


-- just a helper to be able to take the purchaseshistory

freqListCar name cart []=[]
freqListCar name cart ((user,userlist):t) = if (name==user) then gocount (go_display cart userlist) else freqListCar name cart t
--it gofind for every element in the cart and return a single list 
go_display [] userlist=[]
go_display (h:t) userlist=(gofind userlist h [])++(go_display t userlist)


freqListCartAndItems name cart=gocount ((go_fok_the_list (freqListItems name))++(go_fok_the_list (freqListCart name cart)))

freqListUsers name=freqListUsers1 name purchasesHistory

-- removes the user list from the list of purchases to be able to make intersection
freqListUsers1 name []=[]
freqListUsers1 name ((user,list):t)= if name==user then gocount (go_fok_the_list (gogetlist [] (purchasesIntersection (go_find_list_by_name_from_stats name (getAllUsersStats purchasesHistory)) (removefromlist ((name,go_find_list_by_name_from_stats name (getAllUsersStats purchasesHistory))) (getAllUsersStats purchasesHistory))))) else freqListUsers1 name t
--freqListUsers1 name ((user,list):t)= if name==user then gocount (go_fok_the_list (gogetlist [] (purchasesIntersection list (removefromlist (name,list) purchasesHistory)))) else freqListUsers1 name t
--freqListUsers1 name ((user,list):t)=

gogetlist acc []=acc
gogetlist acc (h:t)=gogetlist (acc++go_merge [] h) t 


go_merge acc []=acc
go_merge acc ((item,list):t)=(go_merge (acc++list) t)


go_find_list_by_name_from_stats name []=[]
go_find_list_by_name_from_stats name ((user,list):t)=if name==user then list else go_find_list_by_name_from_stats name t



recommendEmptyCart n=if (find_name n purchasesHistory)==[] then [] else (go_fok_the_list (freqListItems n))!!(randomZeroToX (length ((go_fok_the_list (freqListItems n)))-1))


find_name name []=[]
find_name name ((user,list):t)=if user==name then list else find_name name t


recommendBasedOnItemsInCart name cart=if find_name name purchasesHistory==[] then [] else (go_fok_the_list (freqListCartAndItems name cart))!!(randomZeroToX (length ((go_fok_the_list (freqListCartAndItems name cart)))-1))


recommendBasedOnUsers name=if length (freqListUsers name)==0 then [] else (go_fok_the_list (freqListUsers name))!!(randomZeroToX (length ((go_fok_the_list (freqListUsers name)))-1))


recommend name cart=if length (recommendBasedOnItemsInCart name cart) ==0 && length (recommendBasedOnUsers name)==0 then items !!(randomZeroToX ((length items)-1)) else if (randomZeroToX 1)==0 then recommendBasedOnItemsInCart name cart  else recommendBasedOnUsers name











