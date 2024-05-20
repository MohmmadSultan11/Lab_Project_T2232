DSC 200 Lab Project -Term 2232
================
2024-05-20

**Student Name:Mohmmad Sultan**

**Student ID: 2210002586**

**Deadline:** 23:59 on Sunday, 19 May 2024

**Total Points:** 20

## Loading Packages

``` r
library(tidyverse)
library(openintro)
library(ggrepel)
```

## Tasks

\`1. (2 points)

``` r
nrow(seattlepets)
```

    ## [1] 52519

Task 1: How many pets are included in this dataset?

\`2. (2 points)

``` r
ncol(seattlepets)
```

    ## [1] 7

Task 2: How many variables do we have for each pet?

\`3. (2 points)

``` r
seattlepets %>%
  count(species)
```

    ## # A tibble: 4 × 2
    ##   species     n
    ##   <chr>   <int>
    ## 1 Cat     17294
    ## 2 Dog     35181
    ## 3 Goat       38
    ## 4 Pig         6

Task 3: What are the pet species in Seattle? How many pets are there for
each species?

\`4. (2 points)

``` r
seattlepets %>%
  count(animal_name) %>%
  arrange(desc(n)) %>%
  head(10)
```

    ## # A tibble: 10 × 2
    ##    animal_name     n
    ##    <chr>       <int>
    ##  1 <NA>          483
    ##  2 Lucy          439
    ##  3 Charlie       387
    ##  4 Luna          355
    ##  5 Bella         331
    ##  6 Max           270
    ##  7 Daisy         261
    ##  8 Molly         240
    ##  9 Jack          232
    ## 10 Lily          232

Task 4: What are the ten most common pet names?

\`5. (2 points)

``` r
library(dplyr)

# Retrieve and display all the 6 records for the species Pig sorted by pet names
pigs <- seattlepets %>%
  filter(species == "Pig") %>%
  arrange(animal_name)

# Display the result
print(pigs)
```

    ## # A tibble: 6 × 7
    ##   license_issue_date license_number animal_name species primary_breed
    ##   <date>             <chr>          <chr>       <chr>   <chr>        
    ## 1 2018-04-23         S116433        Atticus     Pig     Pot-Bellied  
    ## 2 2018-08-29         S146305        Coconut     Pig     Pot-Bellied  
    ## 3 2018-04-10         139975         Darla       Pig     Pot Bellied  
    ## 4 2018-07-27         731834         Millie      Pig     Pot-Bellied  
    ## 5 2018-08-29         S146306        Othello     Pig     Pot-Bellied  
    ## 6 2018-05-12         S141788        <NA>        Pig     Standard     
    ## # ℹ 2 more variables: secondary_breed <chr>, zip_code <chr>

\`6. (2 points)

``` r
# Retrieve and display ONLY the pet name (animal_name) and primary_breed of the species Goat sorted by pet names
goats <- seattlepets %>%
  filter(species == "Goat") %>%
  select(animal_name, primary_breed) %>%
  arrange(animal_name)

# Display the result
print(goats)
```

    ## # A tibble: 38 × 2
    ##    animal_name     primary_breed
    ##    <chr>           <chr>        
    ##  1 Abelard         Miniature    
    ##  2 Aggie           Miniature    
    ##  3 Arya            Miniature    
    ##  4 Beans           Miniature    
    ##  5 Brussels Sprout Miniature    
    ##  6 Darcy           Miniature    
    ##  7 Fawn            Miniature    
    ##  8 Fiona           Miniature    
    ##  9 Gavin           Standard     
    ## 10 Grace           Miniature    
    ## # ℹ 28 more rows

\`7. (2 points)

``` r
# Concatenate the two columns animal_name and species into a single column named pet
# then display license_number and pet sorted by pet
pets <- seattlepets %>%
  mutate(pet = paste(animal_name, species, sep = "; ")) %>%
  select(license_number, pet) %>%
  arrange(pet)

# Display the result
print(pets)
```

    ## # A tibble: 52,519 × 2
    ##    license_number pet                                    
    ##    <chr>          <chr>                                  
    ##  1 8001665        "\"Luci\" Lucia Rosalin Wicksugal; Dog"
    ##  2 896557         "\"Mama\" Maya; Cat"                   
    ##  3 S147119        "\"Mo\"; Cat"                          
    ##  4 353597         "'Alani; Cat"                          
    ##  5 S143106        "'Murca; Dog"                          
    ##  6 573722         "-; Cat"                               
    ##  7 S126229        "1; Cat"                               
    ##  8 S126230        "2; Cat"                               
    ##  9 133239         "30 Weight; Cat"                       
    ## 10 S142492        "7's; Dog"                             
    ## # ℹ 52,509 more rows

\`8. (2 points)

``` r
library(ggplot2)

# Plot the counts of the species as bars
ggplot(seattlepets, aes(x = species)) +
  geom_bar() +
  labs(title = "Counts of Pet Species in Seattle", x = "Species", y = "Count")
```

![](Lab_project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

\`9. (2 points)

``` r
top_10_names <- seattlepets %>% 
filter(animal_name %in% c( "Lucy"  , "Charlie" , "Luna" , "Bella" , "Max"    , 
                           "Daisy" , "Molly"   , "Jack" , "Lily"  , "Stella" ))
top_10_names
```

    ## # A tibble: 2,974 × 7
    ##    license_issue_date license_number animal_name species primary_breed          
    ##    <date>             <chr>          <chr>       <chr>   <chr>                  
    ##  1 2018-11-25         S120480        Charlie     Dog     Retriever, Labrador    
    ##  2 2018-11-03         829563         Max         Dog     Retriever, Labrador    
    ##  3 2018-10-29         732106         Lily        Cat     Domestic Shorthair     
    ##  4 2018-11-25         895808         Max         Cat     Domestic Shorthair     
    ##  5 2018-11-26         834841         Daisy       Dog     Terrier, American Pit …
    ##  6 2018-12-13         8003804        Charlie     Dog     Border Collie          
    ##  7 2018-11-06         S125292        Jack        Cat     Domestic Shorthair     
    ##  8 2018-11-01         835179         Stella      Dog     Retriever, Labrador    
    ##  9 2018-12-14         950094         Molly       Dog     Retriever, Labrador    
    ## 10 2018-11-24         S137301        Lucy        Dog     Hound                  
    ## # ℹ 2,964 more rows
    ## # ℹ 2 more variables: secondary_breed <chr>, zip_code <chr>

\`a. What does the above code chunk do? Filter the Dataset: It filters
the seattlepets dataset to include only rows where the animal_name
column matches one of the specified names in the list: “Lucy”,
“Charlie”, “Luna”, “Bella”, “Max”, “Daisy”, “Molly”, “Jack”, “Lily”, and
“Stella”. Assign the Result to top_10_names: The filtered dataset is
then stored in a new variable called top_10_names. Display the Result:
It displays the contents of top_10_names to show the filtered data

\`b. Plot the counts of the pet names (animal_name) in top_10_names

``` r
ggplot(top_10_names, aes(x = animal_name, fill = species)) +
  geom_bar() +
  labs(title = "Top 10 Pet Names in Seattle Segmented by Species", 
       x = "Pet Name", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Lab_project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

\`10. (2 points)

\`The below code plots the proportion of dogs with a given name versus
the proportion of cats with the same name. The 20 most common cat and
dog names are displayed. The diagonal line on the plot is the x = y
line; if a name appeared on this line, the name’s popularity would be
exactly the same for dogs and cats.

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning in geom_image(mapping, data, inherit.aes = inherit.aes, na.rm = na.rm, : All aesthetics have length 1, but the data has 20 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.
    ## All aesthetics have length 1, but the data has 20 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

![](Lab_project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

\`What names are more common for cats than dogs? The ones above the line
or the ones below the line?

\`Answer here.The names that are more common for cats than dogs are the
ones above the line. This is because the y-axis represents the
proportion of dogs and the x-axis represents the proportion of cats.
Points above the line y=x indicate that the proportion for cats is
higher than for dogs.

\`Is the relationship between the two variables (proportion of cats with
a given name and proportion of dogs with a given name) positive or
negative? What does this mean in context of the data?

\`Answer here The relationship between the two variables (proportion of
cats with a given name and proportion of dogs with a given name) is
positive. This means that, generally, if a name is popular among cats,
it also tends to be popular among dogs. The points are scattered around
the line

y=x, indicating that the same names are often used for both cats and
dogs, with varying degrees of popularity.
