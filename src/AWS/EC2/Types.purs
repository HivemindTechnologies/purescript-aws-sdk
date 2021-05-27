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

{- 
Depending on which Attribute is sent in the request the response can contain different keys 
Examples: 

with Attribute.KernelId
{
  Groups: [],
  BlockDeviceMappings: [],
  InstanceId: 'i-xxxxx',
  KernelId: {},
  ProductCodes: []
}

with Attribute.InstanceType
{
  Groups: [],
  BlockDeviceMappings: [],
  InstanceId: 'i-xxxx',
  InstanceType: { Value: 'm5.2xlarge' },
  ProductCodes: []
}
-}
type Raw a
  = ( "InstanceId" :: String | a )

type InstanceTypeAttribute a
  = { "InstanceType" :: { "Value" :: String }
    | Raw a
    }
