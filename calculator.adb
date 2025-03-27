with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;

use Ada.Text_IO;
use Ada.Integer_Text_IO;

procedure Reworked_Calculator is

   -- Yeni isimlendirmeler
   user_choice      : Integer;
   sum_of_integers  : Integer;
   next_value       : Integer;
   square_val       : Integer;  -- Kodda fiilen kullanılmıyor ama tutuldu
   power_val        : Integer;  -- Kodda fiilen kullanılmıyor ama tutuldu
   exponent_val     : Integer;  -- Kodda fiilen kullanılmıyor ama tutuldu
   number_count     : Natural;
   float_a          : Float;
   float_b          : Float;
   float_result     : Float;

   subtype PositiveFloat is Float range 0.0 .. Float'Last;
   test_value  : PositiveFloat;
   final_value : PositiveFloat;

begin
   Put_Line("=== Welcome to the Ada-Based Calculator ===");
   New_Line;
   Put_Line("Select one of the following operations:");
   New_Line;
   
   -- Menü seçeneklerini ekrana basıyoruz
   Put_Line("1) Sum multiple integers");
   Put_Line("2) Add two floating-point numbers");
   Put_Line("3) Subtract two floating-point numbers");
   Put_Line("4) Multiply two floating-point numbers");
   Put_Line("5) Divide two floating-point numbers");
   Put_Line("0) Exit this calculator");
   New_Line;

   -- Kullanıcı 0 girene kadar döngü
   loop
      Put("Please enter your menu selection: ");
      Get(user_choice);
      New_Line(1);
      
      Put_Line("You have chosen: " & user_choice'Img);
      New_Line;

      case user_choice is

         when 1 =>
            Put("Enter how many integers you want to sum: ");
            Get(Item => number_count);
            New_Line;

            sum_of_integers := 0;
            Put("Now enter each integer, separated by spaces: ");

            for i in 1 .. number_count loop
               Get(Item => next_value);
               sum_of_integers := sum_of_integers + next_value;
            end loop;

            Put("The total sum is: " & sum_of_integers'Img);

         when 2 =>
            Put("For a + b, please enter a: ");
            Ada.Float_Text_IO.Get(Item => float_a);
            Put("For a + b, please enter b: ");
            Ada.Float_Text_IO.Get(Item => float_b);

            float_result := float_a + float_b;
            Put("Result of addition is: ");
            Ada.Float_Text_IO.Put(Item => float_result, Fore => 1, Aft => 4, Exp => 0);
            New_Line;

         when 3 =>
            Put("For b - a, please enter b: ");
            Ada.Float_Text_IO.Get(Item => float_b);
            Put("For b - a, please enter a: ");
            Ada.Float_Text_IO.Get(Item => float_a);

            float_result := float_b - float_a;
            Put("Result of subtraction is: ");
            Ada.Float_Text_IO.Put(Item => float_result, Fore => 1, Aft => 4, Exp => 0);
            New_Line;

         when 4 =>
            Put("For a * b, please enter a: ");
            Ada.Float_Text_IO.Get(Item => float_a);
            Put("For a * b, please enter b: ");
            Ada.Float_Text_IO.Get(Item => float_b);

            float_result := float_a * float_b;
            Put("Result of multiplication is: ");
            Ada.Float_Text_IO.Put(Item => float_result, Fore => 1, Aft => 4, Exp => 0);
            New_Line;

         when 5 =>
            Put("For b / a, please enter b: ");
            Ada.Float_Text_IO.Get(Item => float_b);
            Put("For b / a, please enter a: ");
            Ada.Float_Text_IO.Get(Item => float_a);

            float_result := float_b / float_a;
            Put("Result of division is: ");
            Ada.Float_Text_IO.Put(Item => float_result, Fore => 1, Aft => 4, Exp => 0);
            New_Line;

         when 0 =>
            Put_Line("Program is terminating. Goodbye!");
            exit;

         when others =>
            Put_Line("Invalid choice. Please try again.");
      end case;
   end loop;

end Reworked_Calculator;
