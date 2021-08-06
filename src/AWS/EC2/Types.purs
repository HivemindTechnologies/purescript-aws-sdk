module AWS.EC2.Types where

import Prelude (class Show)

data Attribute
  = InstanceType
  | Kernel
  | Ramdisk
  | UserData
  | DisableApiTermination
  | InstanceInitiatedShutdownBehavior
  | RootDeviceName
  | BlockDeviceMapping
  | ProductCodes
  | SourceDestCheck
  | GroupSet
  | EbsOptimized
  | SriovNetSupport
  | EnaSupport
  | EnclaveOptions

instance showAttribute :: Show Attribute where
  show InstanceType = "instanceType"
  show Kernel = "kernel"
  show Ramdisk = "ramdisk"
  show UserData = "userData"
  show DisableApiTermination = "disableApiTermination"
  show InstanceInitiatedShutdownBehavior = "instanceInitiatedShutdownBehavior"
  show RootDeviceName = "rootDeviceName"
  show BlockDeviceMapping = "blockDeviceMapping"
  show ProductCodes = "productCodes"
  show SourceDestCheck = "sourceDestCheck"
  show GroupSet = "groupSet"
  show EbsOptimized = "ebsOptimized"
  show SriovNetSupport = "sriovNetSupport"
  show EnaSupport = "enaSupport"
  show EnclaveOptions = "enclaveOptions"

type InstanceAttributeInstanceType
  = { "InstanceId" :: String
    , "InstanceType" ::
        { "Value" :: String
        }
    }

type VcpuInfo
  = { "DefaultVCpus" :: Int
    , "DefaultCores" :: Int
    , "DefaultThreadsPerCore" :: Int
    }

type InstanceTypeParams
  = { "InstanceType" :: String
    , "VCpuInfo" :: VcpuInfo
    , "MemoryInfo" ::
        { "SizeInMiB" :: Int
        }
    }

type DescribeInstanceTypesResponse
  = { "InstanceTypes" :: Array InstanceTypeParams
    }
