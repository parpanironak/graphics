(*Ronak Parnani
N16003622
HW 3*)

(*##################PART 1.3 requirnments#####################*)

signature HOTELRESERVATIONS = 
sig
	datatype roomconfig = Double_bed | King |Queen
	type resrecord = {uid:int,fname:string,lname:string,cdate:int,nights:int,occu:int,rc:roomconfig}
	type ressys	
	
	
	val empty : int -> int -> int -> ressys
	val reserve : ressys -> resrecord -> ressys
	val cancel : ressys -> int -> ressys
	val getInventory : ressys -> roomconfig -> int -> int
	val getInventorySpan : ressys -> roomconfig -> int -> int -> bool
	val completedStays : ressys -> int -> int
	val removeCompletedStays : ressys -> int -> ressys
end

structure HotelReservations:> HOTELRESERVATIONS=
struct
	datatype roomconfig = Double_bed | King |Queen
	type resrecord={uid:int,fname:string,lname:string,cdate:int,nights:int,occu:int,rc:roomconfig}
	type ressys={d:int,k:int,q:int,reclist: resrecord list}
	
	exception NotEnoughRoomInventory
	exception ExistingID
	exception NotExistingID	
		
	(*empty returns a reservation system with empty list of records*)	
	fun empty d k q =
		let val r={d=d,k=k,q=q,reclist=[]}:ressys;
		in
		r
		end
	
	(*fun count is a helper function for getInventory and getSPan. 
	It takes list of reservation records, room configuration and 
	date as inputs and returns the no. of rooms of a particular 
	type that are occupied on a particular date*)
	fun count ([]:resrecord list, config, date) = 0
		|count ((x::xs):resrecord list, config, date) = if #cdate x <= date andalso #cdate x + #nights x > date andalso #rc x = config then 1 + count (xs, config, date) 
								else count (xs, config, date)
								
	(*getInventory is a function that gives the available inventory.
	It takes reservation system, room conficurationg and date as inputs
	and returns the no of rooms of a particular configuration available 
	on a particular day *)							
	fun getInventory sys config date =
		let val x:roomconfig=config
		val s:ressys=sys
		val z:int=date
		val w:int =count(#reclist s, x, z)
		in
		if config=Double_bed then #d s-w else if config = King then #k s-w else #q s-w 
		end;
	
	(*getSpan is a helper function for getInventorySpan
	it takes list of reservation records, room configuration, total no of rooms of 
	a particular configuration, date and span and returns if the room of the given type is available
	on a particular date. It checks the count of rooms occupied of a type is less than no of total no
	of rooms for the same type for all the days in a given span*)
	fun getSpan (recs:resrecord list, config, no, date, span) = 
			let val c:roomconfig=config 
			val d:int=date
			val b:bool=((no-count(recs,c,d))>0)
			in
				if span=0 then true
				else b andalso getSpan(recs, c, no, d+1,span-1)
			end;
	
	(*getInventorySpan returns true if a room of a particular config is available for all days of given span
	returns false for if not. It takes input as reservation system, room configuration, date, and span*)
	fun getInventorySpan sys config date span = 
		let val x:ressys=sys
		val y:roomconfig=config
		val z:int=date
		val w:int=span
		in
			if config=Double_bed then getSpan(#reclist x, y, #d x, z, w)
			else if config = King then getSpan(#reclist x, y, #k x, z, w)
			else getSpan(#reclist x, config, #q x, z, w)
		end;
		
	(*fun checkid checks if an id already exists in the list of records
	It takes parameters resrecord list and int and returns a boolean
	It is a helper function for reserve*)	
	fun checkid ([]:resrecord list, id)=true     (*returns true if id not found else false*)
		| checkid ((x::xs):resrecord list, id)=
			if #uid x = id then false 
			else checkid(xs,id);		
	
	(*fun reserve takes reservations system and a single reservation record as input 
	and adds the record to the list of reservation record list in the reservation system
	It checks if the id doesnt exist in the system, inventory is available of the entire span of desired stay
	and restrictions of minnights and occupancy are satisfied*)
	fun reserve sys recr=
		let val x:ressys=sys
		val y:resrecord=recr
		val b1:bool=checkid(#reclist x, #uid recr)
		val b2:bool=getInventorySpan x (#rc y) (#cdate y) (#nights y) 
		in
			if b1 andalso b2 then 
			{d=(#d x),k=(#k x),q=(#q x),reclist=(y::(#reclist x))}:ressys
			else x
		end

	
	(*fun remove is used to remove a record from the list of records, having a particular uid
	It is a helper function for cancel*)	
	fun remove ([]:resrecord list, id:int)=[]
		| remove ((x::xs):resrecord list, id:int)=
			if (#uid x)=id then xs
			else x::remove(xs,id)
			
	(*fun cancel is a function that takes input as reservation system and id and removes a recod with that id*)	
	fun cancel sys id=
		let val x:ressys=sys
		val y:int=id
		val b1:bool=checkid(#reclist x, y)

		in
			if b1 then (*if b1 is true id doesnot exist*)
			x
			else {d=(#d x),k=(#k x),q=(#q x),reclist=remove(#reclist x,y)}:ressys
			
		end	
		
	(*Following function is a helper function for completedStays function it takes 
	reservation record list and date as inputs and recursively counts the number of records 
	that have end of stay less than the given date*)	
	fun countCompletedStays([]:resrecord list,date:int)=0
		|countCompletedStays((x::xs):resrecord list,date:int)=
			if date < ((#cdate x) + (#nights x))
			then countCompletedStays(xs,date)
			else 1+countCompletedStays(xs,date)
		
		(*Following function takes reservation system and date as input
	and uses helper function countCompletedStays*)	
	fun completedStays sys date=
		let val x:ressys=sys
		val y:int=date
		in
		countCompletedStays(#reclist x, y)
		end;
	
	(*Following is th ehelper function for the function removeCompletedStays
	It recousively calls itself untill it to remove each old record from the list
	untill it gets an empty list. It returns a list of records after removing the old ones*)
	fun removeOldRec ([]:resrecord list, date:int)=[]
		| removeOldRec ((x::xs):resrecord list, date:int)=
			if (#cdate x + #nights x) <=date
			then removeOldRec(xs,date)
			else x::removeOldRec(xs,date);	
	
	(*removecompletedstays is a function that thakes input as reservations system and date and 
	returns a system after deleting the all records having stays completed the stays before a the given date*)
	fun removeCompletedStays sys date=
		let val x:ressys=sys
		val y:int=date
		in
			{d=(#d x),k=(#k x),q=(#q x),reclist=removeOldRec(#reclist x,y)}:ressys
		end

end



signature ROOMDETAIL =
sig
	val doubleAvailable : int
	val kingAvailable : int
	val queenAvailable : int
	val minnights : int option
	val occupancyLimit : int
end;

(*##################PART 1.4 More Requirnments#####################*)

signature HOTELRESERVATIONS2 = 
sig
	datatype roomconfig = Double_bed | King |Queen
	type resrecord ={uid:int,fname:string,lname:string,cdate:int,nights:int,occu:int,rc:roomconfig}
	type ressys
	
	
	exception NotEnoughRoomInventory (*raised when there is not enought rooms of a particular type available*)
	exception ExistingID (*raised when inserting the record with id that already exists*)
	exception NotExistingID	(*raised when finding an id that doesnot exsit in the system*)
	exception RestrictionsNotSatisfied (*raised when restictions minnights and and occupancylimit not satisfied*)
	
	val restrictions : resrecord -> bool	
	val empty :ressys
	val reserve : ressys -> resrecord -> ressys
	val cancel : ressys -> int -> ressys
	val getInventory : ressys -> roomconfig -> int -> int
	val getInventorySpan : ressys -> roomconfig -> int -> int -> bool
	val completedStays : ressys -> int -> int
	val removeCompletedStays : ressys -> int -> ressys
	val guestQuantity: ressys -> int -> int
	val upgrade: ressys -> int -> roomconfig -> ressys
end


functor MakeHotel (structure R : ROOMDETAIL) :>
sig
	include HOTELRESERVATIONS2
	
end =
struct	
	datatype roomconfig = Double_bed | King |Queen
	type resrecord={uid:int,fname:string,lname:string,cdate:int,nights:int,occu:int,rc:roomconfig};
	type ressys={d:int,k:int,q:int,reclist: resrecord list};
	 
	 
	 
	exception NotEnoughRoomInventory (*raised when there is not enought rooms of a particular type available*)
	exception ExistingID	(*raised when inserting the record with id that already exists*)
	exception NotExistingID	(*raised when finding an id that doesnot exsit in the system*)
	exception RestrictionsNotSatisfied (*raised when restictions minnights and and occupancylimit not satisfied*)
	
	val empty={d=R.doubleAvailable,k=R.kingAvailable,q=R.queenAvailable,reclist=[]}:ressys; (*reservation system without any records*)
	
	(*fun count is a helper function for getInventory and getSPan. 
	It takes list of reservation records, room configuration and 
	date as inputs and returns the no. of rooms of a particular 
	type that are occupied on a particular date*)
	fun count ([]:resrecord list, config, date) = 0
		|count ((x::xs):resrecord list, config, date) = if #cdate x <= date andalso #cdate x + #nights x > date andalso #rc x = config then 1 + count (xs, config, date) 
								else count (xs, config, date);
								
	(*getInventory is a function that gives the available inventory.
	It takes reservation system, room conficurationg and date as inputs
	and returns the no of rooms of a particular configuration available 
	on a particular day *)							
	fun getInventory sys config date =
		let val x:roomconfig=config
		val s:ressys=sys
		val z:int=date
		val w:int =count(#reclist s, x, z)
		in
		if config=Double_bed then #d s-w else if config = King then #k s-w else #q s-w 
		end;
	
	(*getSpan is a helper function for getInventorySpan
	it takes list of reservation records, room configuration, total no of rooms of 
	a particular configuration, date and span and returns if the room of the given type is available
	on a particular date. It checks the count of rooms occupied of a type is less than no of total no
	of rooms for the same type for all the days in a given span*)
	fun getSpan (recs:resrecord list, config, no, date, span) = 
			let val c:roomconfig=config 
			val d:int=date
			val b:bool=((no-count(recs,c,d))>0)
			in
				if span=0 then true
				else b andalso getSpan(recs, c, no, d+1,span-1)
			end;
	
	(*getInventorySpan returns true if a room of a particular config is available for all days of given span
	returns false for if not. It takes input as reservation system, room configuration, date, and span*)
	fun getInventorySpan sys config date span = 
		let val x:ressys=sys
		val y:roomconfig=config
		val z:int=date
		val w:int=span
		in
			if config=Double_bed then getSpan(#reclist x, y, #d x, z, w)
			else if config = King then getSpan(#reclist x, y, #k x, z, w)
			else getSpan(#reclist x, config, #q x, z, w)
		end;
		
	(*fun checkid checks if an id already exists in the list of records
	It takes parameters resrecord list and int and returns a boolean
	It is a helper function for reserve*)	
	fun checkid ([]:resrecord list, id)=true     (*returns true if id not found else false*)
		| checkid ((x::xs):resrecord list, id)=
			if #uid x = id then false 
			else checkid(xs,id);
	
	(*following function is a helper function for restrictions.
	Takes span as input and compares it with minimum no of nights 
	Returns true if its equal to minnights or minnights is NONE.
	This function makes a recursive call to itself with reducing 
	the value of spane by one. If the value of span reaches 0 
	it returns false*)
	fun restrictnights n = 
		if n = 0 then false
		else if R.minnights = SOME n then true
		else if R.minnights = NONE then true
		else restrictnights (n-1);
	
	(*fun restrictions take reservation record as input to check if it satisfies 
	restrictions on minimum number of nights as well as maximum no of occupancy
	returns true only if it satisfies both the conditions else returns false*)
	
	fun restrictions recr =
		let val x:resrecord=recr
		in
		if (#occu x) <= R.occupancyLimit then
		restrictnights (#nights recr)
		else false			
		end;	
		
	(*fun reserve takes reservations system and a single reservation record as input 
	and adds the record to the list of reservation record list in the reservation system
	It checks if the id doesnt exist in the system, inventory is available of the entire span of desired stay
	and restrictions of minnights and occupancy are satisfied*)
	fun reserve sys recr=
		let val x:ressys=sys
		val y:resrecord=recr
		val b1:bool=checkid(#reclist x, #uid recr)
		val b2:bool=getInventorySpan x (#rc y) (#cdate y) (#nights y) 
		val b3:bool=restrictions recr
		in
			if b1 then if b2 then if b3 then
			{d=(#d x),k=(#k x),q=(#q x),reclist=(y::(#reclist x))}:ressys
			else raise RestrictionsNotSatisfied
			else raise NotEnoughRoomInventory
			else raise ExistingID
		end;

	
	(*fun remove is used to remove a record from the list of records, having a particular uid
	It is a helper function for cancel*)
	fun remove ([]:resrecord list, id:int)=[]
		| remove ((x::xs):resrecord list, id:int)=
			if (#uid x)=id then xs
			else x::remove(xs,id);
	
	(*fun cancel is a function that takes input as reservation system and id and removes a recod with that id*)
	fun cancel sys id=
		let val x:ressys=sys
		val y:int=id
		val b1:bool=checkid(#reclist x, y)

		in
			if b1 then (*if b1 is true id doesnot exist*)
			raise NotExistingID 
			else {d=(#d x),k=(#k x),q=(#q x),reclist=remove(#reclist x,y)}:ressys
			
		end	;
		
	(*Following function is a helper function for completedStays function it takes 
	reservation record list and date as inputs and recursively counts the number of records 
	that have end of stay less than the given date*)	
	fun countCompletedStays([]:resrecord list,date:int)=0
		|countCompletedStays((x::xs):resrecord list,date:int)=
			if date < ((#cdate x) + (#nights x))
			then countCompletedStays(xs,date)
			else 1+countCompletedStays(xs,date);
	
	(*Following function takes reservation system and date as input
	and uses helper function countCompletedStays*)	
	fun completedStays sys date=
		let val x:ressys=sys
		val y:int=date
		in
		countCompletedStays(#reclist x, y)
		end;
	
	(*Following is th ehelper function for the function removeCompletedStays
	It recousively calls itself untill it to remove each old record from the list
	untill it gets an empty list. It returns a list of records after removing the old ones*)
	fun removeOldRec ([]:resrecord list, date:int)=[]
		| removeOldRec ((x::xs):resrecord list, date:int)=
			if (#cdate x + #nights x) <=date
			then removeOldRec(xs,date)
			else x::removeOldRec(xs,date);	
		
	(*removecompletedstays is a function that thakes input as reservations system and date and 
	returns a system after deleting the all records having stays completed the stays before a the given date*)
	fun removeCompletedStays sys date=
		let val x:ressys=sys
		val y:int=date
		in
			{d=(#d x),k=(#k x),q=(#q x),reclist=removeOldRec(#reclist x,y)}:ressys
		end;
	
	(*Following function counts the number of guests in a hotel 
	on a particular day by iterating through each record
	in the record list and checking if the date in the span of each stay
	It is a helper function for function guestQuantity*)
	fun countGuests ([]:resrecord list, date:int)=0
		| countGuests((x::xs):resrecord list, date:int)= 
		if date >= (#cdate x) andalso date < (#cdate x) + (#nights x) then (#occu x) + countGuests(xs,date)
		else countGuests(xs,date)
	
	(*returns the no. of guests staying in a hotel on a particular day*)
	fun guestQuantity sys date=
		let val x:ressys = sys
		val y:int = date
		in
			countGuests ((#reclist sys),date)
		end;
	
	(*following is a helper function for upgrade feature
		It checks list of records for a given id and returns that record*)
	fun getRecord ([]:resrecord list, id:int ) = raise NotExistingID	
		| getRecord ((x::xs):resrecord list, id:int) = 
		if (#uid x) = id then x 
		else getRecord (xs,id)
	
	(*following function takes input as reservation record and roomconfig, 
	changes the configuration of that record and returns that record
	It is a helper function for the upgrade feature*)
	fun updateConfig (x:resrecord,config:roomconfig)=
		{uid=(#uid x),fname=(#fname x),lname=(#lname x),cdate=(#cdate x),nights=(#nights x),occu=(#occu x),rc=config}:resrecord;
	
	(*following function takes list of records, id and roomconfig as input and 
	returns the list of records after changing roomconfig of one record in the list to
	a specified one
	It is a helper function for the upgrade feature*)
	fun change ([]:resrecord list, id:int , config:roomconfig) = raise NotExistingID	
		| change ((x::xs):resrecord list, id:int , config:roomconfig) = 
			if (#uid x) = id then updateConfig(x,config)::xs 
			else x::change(xs,id,config)
		
	(*Function upgrade is a additional feature
	It is used to change the roomconfig of a reservation if there exists a room of a new type
	for the same duration. It takes reservation system, id and new room config as inputs
	It checks if the record of that id already exists, it uses getInventorySpan to check if roomtype is available
	and then changes the record. Upgrade accepts any roomtype including the one that has already been reserved. 
	Example if the rooms of type king are all reserved, and an upgrade on a record with roomconfig King
	is requested to change to config to King again, system will not reallocate a room but see that all the King rooms are taken
	and will raise notenoughroominventory exeption. The system will remain unchanged. However if the king rooms were availbale
	then the system will reallocate it but will give the same end result *)
	fun upgrade sys id config=
		let val x:ressys = sys
		val y:int = id
		val z:roomconfig = config
		val w:resrecord = getRecord(#reclist x, y)
		in
			if getInventorySpan sys config (#cdate w) (#nights w) then  {d=(#d x),k=(#k x),q=(#q x),reclist= change((#reclist x), y , z)}:ressys
			else raise NotEnoughRoomInventory
		end;
		
end;	


(*##################PART 1.5 Deliverables#####################*)

(*Instantiating the srtucture HiltonRoomDetail and specifying no. of 
rooms of each type as well and minnights and occupancy limit*)
structure HiltonRoomDetail =
struct
val doubleAvailable = 5
val kingAvailable = 3
val queenAvailable = 3
val minnights = SOME 2
val occupancyLimit = 4 
end;

structure HR =
MakeHotel(structure R = HiltonRoomDetail);

open HR (*opening the structure to access its values and functions*)
val HRsys = empty (*assigning empty to Hotel Reservation system variable*)

(*Initializing reservation recorda*)
val r1={uid=1,fname="Jack",lname="Baur",cdate=1,nights=5,occu=3,rc=King}:resrecord;
val r2={uid=2,fname="Michael",lname="Scofield",cdate=2,nights=5,occu=3,rc=King}:resrecord;
val r3={uid=3,fname="Sherlock",lname="Holmes",cdate=3,nights=5,occu=2,rc=King}:resrecord;
val r4={uid=4,fname="Dexter",lname="Morgan",cdate=3,nights=5,occu=2,rc=Double_bed}:resrecord;
val r5={uid=5,fname="Peter",lname="Bishop",cdate=4,nights=5,occu=4,rc=Double_bed}:resrecord;
val r6={uid=6,fname="Carrie",lname="Mathison",cdate=10,nights=1,occu=2,rc=Queen}:resrecord;
val r7={uid=7,fname="Michael",lname="Stonebridge",cdate=11,nights=3,occu=5,rc=Double_bed}:resrecord;
val r8={uid=8,fname="John",lname="Luther",cdate=4,nights=5,occu=3,rc=King}:resrecord;
val r9={uid=9,fname="John",lname="Luther",cdate=8,nights=5,occu=3,rc=Queen}:resrecord;

(*Making 5 reservations in HRsys by calling reserve function*)
val HRsys = reserve HRsys r1;
val HRsys = reserve HRsys r2;
val HRsys = reserve HRsys r3;
val HRsys = reserve HRsys r4;
val HRsys = reserve HRsys r5;

val HRsys = reserve HRsys r6 handle RestrictionsNotSatisfied => ( print "Restiction not satisfied error handled for minnights"; HRsys); (*Demonstrating reserve on record that doesnot meet minnights restriction*)
val HRsys = reserve HRsys r7 handle RestrictionsNotSatisfied => ( print "Restiction not satisfied error handled for max occupancy"; HRsys);  (*Demonstrating reserve on record that doesnot meet maximum occupancy restriction*)
val HRsys = reserve HRsys r8 handle RestrictionsNotSatisfied => HRsys (*Demonstrating reserve on record that requests rooms that are unavailable*)
									| NotEnoughRoomInventory => ( print "Not Enough Room Inventory Handled"; HRsys);
									
val HRsys = reserve HRsys r9
(*Demonstaring canceling of reservations of id 1 and 9*)
val HRsys = cancel HRsys 9
val HRsys = cancel HRsys 1

val kinginventory = getInventory HRsys King  4 (*No. of king beds availabe in date 4*)
val completedbefore = completedStays HRsys 8 (*No. of completedStays on date 8 before removing*)
val HRsys = removeCompletedStays HRsys 8 (*Removing completedStays on date 8*)
val completedafter = completedStays HRsys 8 (*No. of completedStays on date 8 after removing*)

val noofguests = guestQuantity HRsys 4 (*No of guests staying on day four. It will retrun 4 which is no of 
occupants in reocrd 5 since the old records have been removed and ther is only one record left in the sytem*)
val HRsys = upgrade HRsys 5 King (*Upgrading the record with UID 5 form double bed to king*)