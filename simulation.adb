-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
   Number_Of_Products   : constant Integer := 5;
   Number_Of_Recipes : constant Integer := 3;
   Number_Of_Clients  : constant Integer := 2;

   subtype Ordinal_Suffix_Type is String (1 .. 2);
   Ordinal_Suffix : Ordinal_Suffix_Type;

   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Recipe_Type is Integer range 1 .. Number_Of_Recipes;
   subtype Client_Type is Integer range 1 .. Number_Of_Clients;

   Product_Name : constant array (Product_Type) of String (1 .. 6) :=
     ("Flour ", "Water ", "Sugar ", "Yeast ", "Butter");
   Recipe_Name : constant array (Recipe_Type) of String (1 .. 10) :=
     ("Bun       ", "Bread     ", "Shortbread");
   package Random_Recipe is new Ada.Numerics.Discrete_Random (Recipe_Type);

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start (Product : in Product_Type; Production_Time : in Integer);
   end Producer;

   -- Client gets an arbitrary assembly of several products from the buffer
   task type Client is
      -- Give the Client an identity
      entry Start
        (Client_Number : in Client_Type; Consumption_Time : in Integer);
   end Client;

   -- In the Buffer, products are assemblied into an assembly
   task type Storage is
      -- Accept a product to the storage provided there is a room for it
      entry Take (Product : in Product_Type; Number : in Integer);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver (Recipe : in Recipe_Type; Number : out Integer);
   end Storage;

   P : array (1 .. Number_Of_Products) of Producer;
   C : array (1 .. Number_Of_Clients) of Client;
   S : Storage;

   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      G : Random_Production.Generator;   --  generator liczb losowych
      Product_Type_Number : Integer;
      Product_Number      : Integer;
      Production          : Integer;
   begin
      accept Start (Product : in Product_Type; Production_Time : in Integer) do
         Random_Production.Reset (G);    --  start random number generator
         Product_Number      := 1;
         Product_Type_Number := Product;
         Production          := Production_Time;
      end Start;
      Put_Line ("Started producer of " & Product_Name (Product_Type_Number));
      loop
         delay Duration (Random_Production.Random (G)); --  symuluj produkcje
         Put_Line
           ("Produced product " & Product_Name (Product_Type_Number) &
            " number " & Integer'Image (Product_Number));
         -- Accept for storage
         S.Take (Product_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
      end loop;
   end Producer;

   task body Client is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);
      G : Random_Consumption.Generator;  --  random number generator (time)
      G2              : Random_Recipe.Generator;    --  also (assemblies)
      Client_No     : Client_Type;
      Recipe_Count : Integer;
      Consumption     : Integer;
      Recipe_Type   : Integer;
      Client_Name : constant array
        (1 .. Number_Of_Clients) of String (1 .. 6) :=
        ("Alicja", "Bogdan");
   begin
      accept Start
        (Client_Number : in Client_Type; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);   --  ustaw generator
         Random_Recipe.Reset (G2);     --  tez
         Client_No := Client_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line ("Started client " & Client_Name (Client_No));
      loop
         delay Duration
           (Random_Consumption.Random (G)); --  simulate consumption
         Recipe_Type := Random_Recipe.Random (G2);
         -- take an assembly for consumption
         S.Deliver (Recipe_Type, Recipe_Count);

         if Recipe_Count mod 10 = 1 then
            Ordinal_Suffix := "st";
         elsif Recipe_Count mod 10 = 2 then
            Ordinal_Suffix := "nd";
         elsif Recipe_Count mod 10 = 3 then
            Ordinal_Suffix := "rd";
         else
            Ordinal_Suffix := "th";
         end if;    

         if Recipe_Count /= 0 then
            Put_Line
              (Client_Name (Client_No) & " got their fresh " &
               Recipe_Name (Recipe_Type) & " which is the" &
               Integer'Image (Recipe_Count) & Ordinal_Suffix &
               " " & Recipe_Name (Recipe_Type) & " of today.");
         else
            Put_Line
              (Client_Name (Client_No) & " did not get their " &
               Recipe_Name (Recipe_Type) & ".");
         end if;
      end loop;
   end Client;

   task body Storage is
      Storage_Capacity : constant Integer := 30;
      type Storage_type is array (Product_Type) of Integer;
      Storage          : Storage_type := (0, 0, 0, 0, 0);
      Recipe_Content : array (Recipe_Type, Product_Type) of Integer :=
        ((2, 1, 1, 1, 0), (3, 2, 1, 3, 0), (1, 1, 2, 1, 2));
      Max_Product_Needed : array (Product_Type) of Integer;
      Recipe_Count      : array (Recipe_Type) of Integer := (1, 1, 1);
      In_Storage           : Integer                          := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Product_Needed (W) := 0;
            for Z in Recipe_Type loop
               if Recipe_Content (Z, W) > Max_Product_Needed (W) then
                  Max_Product_Needed (W) := Recipe_Content (Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Product_Type) return Boolean is
	 Free: Integer;		--  free room in the storage
	 -- how many products are for production of arbitrary assembly
	 Lacking: array(Product_Type) of Integer;
	 -- how much room is needed in storage to produce arbitrary assembly
	 Lacking_room: Integer;
	 MP: Boolean;			--  can accept
      begin
	 if In_Storage >= Storage_Capacity then
	    return False;
	 end if;
	 -- There is free room in the storage
	 Free := Storage_Capacity - In_Storage;
	 MP := True;
	 for W in Product_Type loop
	    if Storage(W) < Max_Product_Needed (W) then
	       MP := False;
	    end if;
	 end loop;
	 if MP then
	    return True;		--  storage has products for arbitrary
	       				--  assembly
	 end if;
	 if Integer'Max(0, Max_Product_Needed(Product) - Storage(Product)) > 0 then -- czy ty moze byc na minusie
	    -- exactly this product lacks
	    return True;
	 end if;
	 Lacking_room := 1;			--  insert current product
	 for W in Product_Type loop
	    Lacking(W) := Integer'Max(0, Max_Product_Needed(W) - Storage(W));
	    Lacking_room := Lacking_room + Lacking(W);
	 end loop;
	 if Free >= Lacking_room then
	    -- there is enough room in storage for arbitrary assembly
	    return True;
	 else
	    -- no room for this product
	    return False;
	 end if;
      end Can_Accept;

      function Can_Deliver (Recipe : Recipe_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage (W) < Recipe_Content (Recipe, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Product_Type loop
            Put_Line
              ("Storage contents: " & Integer'Image (Storage (W)) & " " &
               Product_Name (W));
         end loop;
      end Storage_Contents;

   begin
      Put_Line ("Storage started");
      Setup_Variables;
      loop
            accept Take (Product : in Product_Type; Number : in Integer) do
               if Can_Accept (Product) then
                  Put_Line
                    ("Accepted product " & Product_Name (Product) &
                     " number " & Integer'Image (Number));
                  Storage (Product) := Storage (Product) + 1;
                  In_Storage        := In_Storage + 1;
               else
                  Put_Line
                    ("Rejected product " & Product_Name (Product) &
                     " number " & Integer'Image (Number));
               end if;
            end Take;
            Storage_Contents;
            accept Deliver (Recipe : in Recipe_Type; Number : out Integer)
            do
               if Can_Deliver (Recipe) then
                  Put_Line
                    ("Delivered result of recipe " & Recipe_Name (Recipe) &
                     " number " & Integer'Image (Recipe_Count (Recipe)));
                  for W in Product_Type loop
                     Storage (W) :=
                       Storage (W) - Recipe_Content (Recipe, W);
                     In_Storage := In_Storage - Recipe_Content (Recipe, W);
                  end loop;
                  Number                     := Recipe_Count (Recipe);
                  Recipe_Count (Recipe) := Recipe_Count (Recipe) + 1;
               else
                  Put_Line
                    ("Lacking products for recipe " &
                     Recipe_Name (Recipe));
                  Number := 0;
               end if;
            end Deliver;
            Storage_Contents;
      end loop;
   end Storage;

begin
   for I in 1 .. Number_Of_Products loop
      P (I).Start (I, 10);
   end loop;
   for J in 1 .. Number_Of_Clients loop
      C (J).Start (J, 12);
   end loop;
end Simulation;
