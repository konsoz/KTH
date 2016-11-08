# coding: utf-8
# Konstantin Sozinov

def dna():          # uppgift 1
    # Hela strängen ska matchas mot någon av ACGT 1 eller flera gånger
    return "^[ACGT]+$"

def sorted():       # uppgift 2
    # Hela strängen ska matchas mot 0 eller flera 9 
    # samt 8,7,6,5,4,3,2,1,0 i den ordning de kommer
    return "^9*8*7*6*5*4*3*2*1*0*$"

def hidden1(x):     # uppgift 3
    # Hela strängen ska matchas mot vad som helst 
    #och en given sträng x i mitten följd av vad som helst
    return "^.*" + x + ".*$"

def hidden2(x):     # uppgift 4
    # Det ska finnas en sekvens av x om man tar bort vissa tecken från strängen
    return  "^.*" + ".*".join(x) + ".*$" 

def equation():     # uppgift 5
    # En ekvation kan antigen börja med +- eller ingen av de
    # Följd av 1 till många siffror
    # (Följd av någon operator samt 1 till många siffror) 0 eller flera gånger
    # Följd kanske av (= samt kanske + eller - följd av 1 till flera siffor
    # Följd av en operator samt 1 till många siffror 0 till 1 gånger)
    return "^[-+]?[0-9]+([-+*/][0-9]+)*(=[+-]?[0-9]+([-+*/][0-9]+)*)?$"

def parentheses():  # uppgift 6
    # Ett till många parentespar som kan innehålla 0 till många parentespar
    # 5 gånger
    return "^(\((\((\((\((\(\))*\))*\))*\))*\))+$"

def sorted3():      # uppgift 7
    # 0 till många siffror i följd av 01 och 2 till 9
    # eller 0 || 1, 2, 3 till 9 etc..
    return "^[0-9]*(01[2-9]|[0-1]2[3-9]|[0-2]3[4-9]|[0-3]4[5-9]|[0-4]5[6-9]|[0-5]6[7-9]|[0-6]7[8-9]|[0-7]89)[0-9]*$"



########################################################################
# Raderna nedan är lite testkod som du kan använda för att provköra
# dina regexar.  Koden definierar en main-metod som läser rader från
# standard input och kollar vilka av de olika regexarna som matchar
# indata-raden.  För de två hidden-uppgifterna används söksträngen
# x="test" (kan lätt ändras nedan).  Du behöver inte sätta dig in i hur
# koden nedan fungerar.
#
# För att provköra från terminal, kör:
# > python s1.py
# Skriv in teststrängar:
# [skriv något roligt]
# ...
########################################################################
from sys import stdin
import re

def main():
    def hidden1_test(): return hidden1('progp')
    def hidden2_test(): return hidden2('progp')
    tasks = [dna, sorted, hidden1_test, hidden2_test, equation, parentheses, sorted3]
    print('Skriv in teststrängar:')
    while True:
        line = stdin.readline().rstrip('\r\n')
        if line == '': break
        for task in tasks:
            result = '' if re.search(task(), line) else 'INTE ' 
            print('%s(): "%s" matchar %suttrycket "%s"' % (task.__name__, line, result, task()))
    

if __name__ == '__main__': main()

    
