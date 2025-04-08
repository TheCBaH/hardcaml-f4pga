(* open Hardcaml *)
open Led

let level_control_test control =
  let test max levels level =
    control ~max ~levels ~level |> Printf.printf "max:%u levels:%u level:%u value:%u\n%!" max levels level
  in
  let levels = [ 15; 14; 11; 8; 4; 1; 0 ] in
  List.iter (test 255 16) levels;
  List.iter (test 25 16) levels;
  List.iter (test 7 16) levels

let%expect_test "level_control_float_test" =
  level_control_test level_control_float;
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:25
      max:25 levels:16 level:14 value:23
      max:25 levels:16 level:11 value:18
      max:25 levels:16 level:8 value:13
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:7
      max:7 levels:16 level:14 value:6
      max:7 levels:16 level:11 value:5
      max:7 levels:16 level:8 value:3
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "level_control_int_test" =
   level_control_test (level_control_int ~scale:1);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:15
      max:25 levels:16 level:14 value:14
      max:25 levels:16 level:11 value:11
      max:25 levels:16 level:8 value:8
      max:25 levels:16 level:4 value:4
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:0
      max:7 levels:16 level:14 value:0
      max:7 levels:16 level:11 value:0
      max:7 levels:16 level:8 value:0
      max:7 levels:16 level:4 value:0
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let%expect_test "level_control_int_test_scaled_4" =
 level_control_test (level_control_int ~scale:4);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:22
      max:25 levels:16 level:14 value:21
      max:25 levels:16 level:11 value:16
      max:25 levels:16 level:8 value:12
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:3
      max:7 levels:16 level:14 value:3
      max:7 levels:16 level:11 value:2
      max:7 levels:16 level:8 value:2
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]

let %expect_test "level_control_int_test_scaled_16" =
level_control_test (level_control_int ~scale:16);
  [%expect
    {|
      max:255 levels:16 level:15 value:255
      max:255 levels:16 level:14 value:238
      max:255 levels:16 level:11 value:187
      max:255 levels:16 level:8 value:136
      max:255 levels:16 level:4 value:68
      max:255 levels:16 level:1 value:17
      max:255 levels:16 level:0 value:0
      max:25 levels:16 level:15 value:24
      max:25 levels:16 level:14 value:22
      max:25 levels:16 level:11 value:17
      max:25 levels:16 level:8 value:13
      max:25 levels:16 level:4 value:6
      max:25 levels:16 level:1 value:1
      max:25 levels:16 level:0 value:0
      max:7 levels:16 level:15 value:6
      max:7 levels:16 level:14 value:6
      max:7 levels:16 level:11 value:4
      max:7 levels:16 level:8 value:3
      max:7 levels:16 level:4 value:1
      max:7 levels:16 level:1 value:0
      max:7 levels:16 level:0 value:0
 |}]


