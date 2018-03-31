val nums = List(0,1,2,3,4,5,6,7,9,10)

def findMissingNumber(nums: List[Int]): Int = {
  nums.reduce(_^_) ^ List.range(1, nums.length + 1).reduce(_^_)
}

val missingNumber = findMissingNumber(nums)
