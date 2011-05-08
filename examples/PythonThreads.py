import thread
import time

# adapted from http://www.tutorialspoint.com/python/python_multithreading.htm

def print_time(name,delay):
   count = 0
   while count < 5:
      time.sleep(delay)
      count += 1
      print "%s: count %d" % (name, count)

def newThread(n):
   thread.start_new_thread(print_time, ("Thread-%d" % n,n))

