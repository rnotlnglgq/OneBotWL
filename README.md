# 基于 Wolfram 语言的 OneBot

基于 OneBot-11（由于目前功能很少，兼容性原则上很高）

## 组件

### OneBot.paclet 程序包

一个简易的SDK，未来可能进行完善。

#### 安装

##### 一次性

在 Wolfram 内核中执行：

```mathematica
PacletInstall["OneBot", "Site"->"https://github.com/rnotlnglgq/OneBotWL/raw/master"]
```

##### 订阅源以便于获取更新

```mathematica
PacletManager`PacletSiteAdd@"https://github.com/rnotlnglgq/OneBotWL/raw/master"
```

```mathematica
PacletInstall["OneBot"]
```

### 用户服务

根据你的需求自行修改 [服务函数库](UserService.wl) 和 [启动脚本](StartListen.wls) 。

#### 已实现特性

* 监听本地端口，响应来自 go-cqhttp 的上报

* 模式匹配消息字符串，进行回复消息的分派：
	* 帮助
	* 表达式计算
	* 绘图
	* TeX排版（需 TeXLive 和 MaTeX 支持）
	* 按步骤积分（需要 [Rubi](https://github.com/RuleBasedIntegration/Rubi) 和 [RubiSteps](https://github.com/asdasd1dsadsa/RubiSteps)（后者已附带在本仓库中） ；使用 TeX 输出，要求同上）
* 对非管理员限制功能。部分功能进行了安全性限制（白名单：[SystemSymbol.wl](OneBot/Kernel/SystemSymbol.wl) [SystemValuedSymbol.wl](OneBot/Kernel/SystemValuedSymbol.wl) ）。
* 打印 Debug 信息（日志）
