# Erlang Distributed Brick Breaker

Running instruction:  
link to guideline video: [Link](https://www.loom.com/share/4c2ce631029a4e73b036102ba427cd90?sid=cbd670c2-0b6c-418f-bc2a-43214a6de7d0)  
Summarized report: [Link]([url](https://docs.google.com/document/d/1Rk0fexd1Q76Rg-UGt_GMKt1bgF1V5gjbHZdPpTChLRk/edit?usp=sharing))
run on each node 'erl -name nodeName -setcookie chosenCookie'  
On the main Node run all those files:   
  * main.erl  
  * main_sup.erl  
  * player_sup.erl  
  * plate.erl  
  * ball.erl  
  
for example: c("main").  
  
to start the game run: 'main:start_game().'  
