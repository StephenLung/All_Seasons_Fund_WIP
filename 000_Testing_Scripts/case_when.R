mass

mtcars


test2 <- function(input = "mpg"){
  
  case_when(input == "mpg" ~ mtcars %>% mutate(new_mpg = mpg +1),
            TRUE ~ mtcars %>% mutate(new_wt = wt +1))
  # 
  # mtcars %>%
  #   case_when((!!mpg_expr) == "mpg" ~ mutate(new_mpg = mpg + 1), #when mpg is selected, a mutate statement is executed
  #             TRUE ~ select(wt)) #otherwise, to select a specific column from mtcars
}

test2(input = "mpg")

test1(input = "mpg")

# if statement 
test1 <- function(input = "mpg"){
  
  if(input == "mpg"){mtcars %>% mutate(new_mpg = mpg +1)}
  else {mtcars %>% select(wt)}
}

test1(input = "mpg")
