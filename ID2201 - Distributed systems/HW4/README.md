# Groupy - a group membership service

In this assignment you will implement a group membership service. If everything works ok, you will not only see colors but the same colors.

This is an assignment were you will implement a group membership service that provides atomic multicast. The aim is to have several application layer processes with a coordinated state i.e. they should all perform the same sequence of state changes. A node that wish to perform a state change must first multicast the change to the group so that all nodes can execute it. Since the multicast layer provides total order, all nodes will be synchronized. The problem in this assignment is that all nodes need to be synchronized even though nodes may come and go (crash). As you will see it is not as trivial as one might first think.



